package org.foundator.link

import java.io._
import HttpMethod._
import java.net.{URI, URL}
import scala.Some
import org.eclipse.jetty.server.handler.AbstractHandler
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.eclipse.jetty.server
import org.json4s.{FieldSerializer, DefaultFormats}
import org.json4s.native.Serialization

abstract class UrlMapper {
    final def url() = addUniqueUrl(StrongUrl(None, None))
    final def url[I, O](method : HttpMethod, handler : Request[I] => Response[O])(implicit manifest : Manifest[I]) = addUniqueUrl(StrongUrl[I, O](None, Some((method, handler, manifest))))
    final def url(parentUrl : StrongUrl[_, _], path : String) = addUniqueUrl(StrongUrl(Some((parentUrl, path)), None))
    final def url[I, O](parentUrl : StrongUrl[_, _], path : String, method : HttpMethod, handler : Request[I] => Response[O])(implicit manifest : Manifest[I]) = addUniqueUrl(StrongUrl[I, O](Some((parentUrl, path)), Some((method, handler, manifest))))

    final def url(base : URL) : StrongUrl[String, Unit] = url(base.toURI)
    final def url(base : URI) : StrongUrl[String, Unit] =
        addUniqueUrl(StrongUrl(
            None,
            Some((GET, (r : Request[String]) => handleDirectory(base, r), implicitly[Manifest[String]])), directory = true))
    final def url(parentUrl : StrongUrl[_, _], path : String, base : URL) : StrongUrl[String, Unit] = url(parentUrl, path, base.toURI)
    final def url(parentUrl : StrongUrl[_, _], path : String, base : URI) : StrongUrl[String, Unit] =
        addUniqueUrl(StrongUrl(
            Some((parentUrl, path)),
            Some((GET, (r : Request[String]) => handleDirectory(base, r), implicitly[Manifest[String]])), directory = true))


    // TODO: Look for a way to avoid side effects for constructing the list of URLs
    private def addUniqueUrl[I, O](url : StrongUrl[I, O]) : StrongUrl[I, O] = {
        val path = getPath(url)
        val method = url.handler.map(_._1)
        if(urls.contains((path, method.map(_.method)))) throw DuplicateUrlException(path, method)
        urls += ((path, method.map(_.method)) -> url)
        url
    }
    private def getPath(url : StrongUrl[_, _]) = {
        def pathOf(url : StrongUrl[_, _]) : List[String] = url match {
            case StrongUrl(None, _, _) => List()
            case StrongUrl(Some((parent, path)), _, _) => path :: pathOf(parent)
        }
        pathOf(url).reverse
    }
    private var urls = Map[(List[String], Option[String]), StrongUrl[_, _]]()

    private[link] def findUrl(method : String, path : List[String]) : Option[(StrongUrl[_, _], List[String])] = {
        for(i <- 0 to path.length) urls.get((path.dropRight(i), Some(method))) match {
            case Some(url) =>
                if(i == 0 || url.directory) return Some((url, path.takeRight(i)))
            case None =>
        }
        None
    }

    private def handleDirectory(base : URI, request : Request[String]) : Response[Unit] = {
        val file = if(request.value.isEmpty || request.value.startsWith("/")) request.value else "/" + request.value
        if(file.toString.contains("/../")) StatusResponse(HttpStatus.NOT_FOUND)
        else {
            try {
                val result = new URL(base.toString + file)
                StreamResponse(HttpStatus.OK, () => result.openStream())
            } catch {
                case e : IOException => StatusResponse(HttpStatus.NOT_FOUND)
            }
        }
    }
}


case class DuplicateUrlException(path : List[String], method : Option[HttpMethod]) extends RuntimeException("Path: " + path + ", method: " + method)


case class StrongUrl[I, O](path : Option[(StrongUrl[_, _], String)], handler : Option[(HttpMethod, Request[I] => Response[O], Manifest[I])], directory : Boolean = false)


case class Request[I](value : I, header : String => Option[String])

sealed abstract class Response[O] {def status : HttpStatus; def headers : List[(String, String)]}
case class StatusResponse[O](status : HttpStatus, headers : List[(String, String)] = List()) extends Response[O]
case class JsonResponse[O](status : HttpStatus, value : O, headers : List[(String, String)] = List()) extends Response[O]
case class StreamResponse[O](status : HttpStatus, inputStream : () => InputStream, headers : List[(String, String)] = List()) extends Response[O]

object StreamResponse {
    def fromFile[O](status : HttpStatus, file : File, headers : Map[String, String] = Map(), cookies : Map[String, String] = Map()) =
        StreamResponse[O](status, () => new BufferedInputStream(new FileInputStream(file)))
}


sealed class HttpMethod(val method : String)

object HttpMethod {
    case object GET extends HttpMethod("GET")
    case object PUT extends HttpMethod("PUT")
    case object POST extends HttpMethod("POST")
    case object DELETE extends HttpMethod("DELETE")
    case object HEAD extends HttpMethod("HEAD")
}


sealed class HttpStatus(val code : Int)

object HttpStatus {
    case object CONTINUE extends HttpStatus(100)
    case object SWITCHING_PROTOCOLS extends HttpStatus(101)
    case object OK extends HttpStatus(200)
    case object NOT_FOUND extends HttpStatus(404)
    case object INTERNAL_SERVER_ERROR extends HttpStatus(500)
}


class UrlMapperHandler(urlMapper : UrlMapper) extends AbstractHandler {

    def handle(httpRequest: HttpServletRequest, httpResponse: HttpServletResponse) : Boolean = {
        val path = if(httpRequest.getPathInfo != null) httpRequest.getPathInfo.split("/").filter(s => s.nonEmpty).toList else List()
        urlMapper.findUrl(httpRequest.getMethod, path) match {
            case Some((StrongUrl(_, handler, directory), subPath)) =>
                handler match {
                    case Some((_, f : (Request[_] => Response[_]), manifest)) =>
                        // TODO: Make request body overrule the query parameter
                        val value = if(directory) subPath.mkString("/") else parseJson(manifest, Option(httpRequest.getParameter("json")).getOrElse("{}"))
                        val request = Request(value, name => Option(httpRequest.getHeader(name)))
                        val response = f(request)
                        respond(httpResponse, response)
                        true
                    case None =>
                        false
                }
            case None =>
                false
        }
    }

    def handle(target: String, baseRequest: server.Request, request: HttpServletRequest, response: HttpServletResponse) {
        baseRequest.setHandled(handle(request, response))
    }

    def respond(httpResponse : HttpServletResponse, response : Response[_]) = response match {
        case StatusResponse(status, headers) =>
            updateResponse(httpResponse, status, headers)
        case JsonResponse(status, value : AnyRef, headers) =>
            updateResponse(httpResponse, status, headers)
            Serialization.write(value, httpResponse.getWriter)(formats)
        case StreamResponse(status, inputStream, headers) =>
            updateResponse(httpResponse, status, headers)
            copyStream(inputStream(), httpResponse.getOutputStream)
    }

    def updateResponse(response: HttpServletResponse, status : HttpStatus, headers : List[(String, String)]) {
        response.setStatus(status.code)
        for((name, value) <- headers) {
            response.setHeader(name, value)
        }
    }

    def parseJson(manifest : Manifest[_], input : String) = {
        Serialization.read(input)(formats, manifest)
    }

    def copyStream(input : InputStream, output : OutputStream) {
        val buffer = new Array[Byte](1024 * 4)
        while(true) {
            val bytesRead = input.read(buffer)
            if(bytesRead == -1) return
            output.write(buffer, 0, bytesRead)
        }
    }

    private val formats = DefaultFormats + FieldSerializer[Object]()
}
