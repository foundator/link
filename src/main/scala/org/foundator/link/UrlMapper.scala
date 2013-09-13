package org.foundator.link

import java.io._
import HttpMethod._
import java.net.{InetSocketAddress, URI, URL}
import scala.Some
import org.eclipse.jetty.server.handler.{ContextHandler, ResourceHandler, AbstractHandler}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.eclipse.jetty.server
import org.json4s.{ParserUtil, MappingException, FieldSerializer, DefaultFormats}
import org.json4s.native.Serialization
import org.eclipse.jetty.util.resource.Resource
import org.eclipse.jetty.server.Server

abstract class UrlMapper {
    def run(port : Int) {
        val server = new Server(port)
        server.setHandler(new UrlMapperHandler(this))
        server.start()
        server.join()
    }

    def run(address : InetSocketAddress) {
        val server = new Server(address)
        server.setHandler(new UrlMapperHandler(this))
        server.start()
        server.join()
    }

    final def url() = addUniqueUrl(StrongUrl(None, None))
    final def url[I, O](method : HttpMethod, handler : Request[I] => Response[O])(implicit manifest : Manifest[I]) = addUniqueUrl(StrongUrl[I, O](None, Some((method, handler, manifest))))
    final def url(parentUrl : StrongUrl[_, _], path : String) = addUniqueUrl(StrongUrl(Some((parentUrl, path)), None))
    final def url[I, O](parentUrl : StrongUrl[_, _], path : String, method : HttpMethod, handler : Request[I] => Response[O])(implicit manifest : Manifest[I]) = addUniqueUrl(StrongUrl[I, O](Some((parentUrl, path)), Some((method, handler, manifest))))
    final def url(base : URL) : StrongUrl[String, Unit] = url(if(base != null) base.toURI else null)
    final def url(base : URI) : StrongUrl[String, Unit] = addUniqueUrl(StrongUrl(None, None, directory = Some(base)), List(HttpMethod.GET, HttpMethod.HEAD))
    final def url(parentUrl : StrongUrl[_, _], path : String, base : URL) : StrongUrl[String, Unit] = url(parentUrl, path, if(base != null) base.toURI else null)
    final def url(parentUrl : StrongUrl[_, _], path : String, base : URI) : StrongUrl[String, Unit] = addUniqueUrl(StrongUrl(Some((parentUrl, path)), None, directory = Some(base)), List(HttpMethod.GET, HttpMethod.HEAD))


    // TODO: Look for a way to avoid side effects for constructing the list of URLs
    private def addUniqueUrl[I, O](url : StrongUrl[I, O], extraMethods : List[HttpMethod] = List()) : StrongUrl[I, O] = {
        if(url.path.exists(_._2.contains("/"))) throw InvalidUrlPartException(url.path.get._2)
        val path = url.absolutePath
        val methods = url.handler.map(_._1) :: extraMethods.map(m => Some(m))
        for(method <- methods) {
            if(urls.contains((path, method.map(_.method)))) throw DuplicateUrlException(path, method)
            urls += ((path, method.map(_.method)) -> url)
        }
        url
    }

    private var urls = Map[(List[String], Option[String]), StrongUrl[_, _]]()

    private[link] def findUrl(method : String, path : List[String]) : Option[(StrongUrl[_, _], List[String])] = {
        for(i <- 0 to path.length) urls.get((path.dropRight(i), Some(method))) match {
            case Some(url) =>
                if(i == 0 || url.directory.isDefined) return Some((url, path.takeRight(i)))
            case None =>
        }
        None
    }

    protected def resource(name : String) : URL = {
        val resource = getClass.getResource(name)
        if(resource != null) {
            val text = resource.toString
            if(resource.getProtocol == "file") {
                val suffix = "/target/classes" + name
                if(text.endsWith(suffix)) {
                    val file = new File(resource.toURI).toString.dropRight(suffix.length)
                    if(new File(file, "pom.xml").exists()) {
                        return new File(new File(file, "src/main/resources"), name).toURI.toURL
                    }
                }
            }
        }
        resource
    }
}


case class DuplicateUrlException(path : List[String], method : Option[HttpMethod]) extends RuntimeException("Path: " + path + ", method: " + method)
case class InvalidUrlPartException(part : String) extends RuntimeException("URL path parts can't contain /: " + part)


case class StrongUrl[I, O](path : Option[(StrongUrl[_, _], String)], handler : Option[(HttpMethod, Request[I] => Response[O], Manifest[I])], directory : Option[URI] = None) {
    val absolutePath = {
        def pathOf(url : StrongUrl[_, _]) : List[String] = url match {
            case StrongUrl(None, _, _) => List()
            case StrongUrl(Some((parent, path)), _, _) => path :: pathOf(parent)
        }
        pathOf(this).reverse
    }

    val url = "/" + absolutePath.mkString("/")
}


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

    def handle(target: String, baseRequest: server.Request, httpRequest: HttpServletRequest, httpResponse: HttpServletResponse) {
        val path = if(httpRequest.getPathInfo != null) httpRequest.getPathInfo.split("/").filter(s => s.nonEmpty).toList else List()
        urlMapper.findUrl(httpRequest.getMethod, path) match {

            case Some((StrongUrl(_, None, Some(directory)), subPath)) =>
                if(directory != null) {
                    baseRequest.setPathInfo("/" + subPath.mkString("/"))
                    val resourceHandler = new ResourceHandler()
                    resourceHandler.setBaseResource(Resource.newResource(directory))
                    resourceHandler.handle(target, baseRequest, httpRequest, httpResponse)
                } else {
                    System.err.println("404 The static directory serving '" + baseRequest.getPathInfo + "' was empty or non-existent when this server was started, and will be treated as empty until the server is restarted.")
                    httpResponse.sendError(404, "Not Found")
                    baseRequest.setHandled(true)
                }

            case Some((StrongUrl(_, Some((_, f : Function1[Request[_], Response[_]], manifest)), None), _)) =>
                val value = try {
                    if(httpRequest.getContentType.matches("application/json([;].*)?")) {
                        parseJson(manifest, httpRequest.getReader)
                    } else {
                        parseJson(manifest, Option(httpRequest.getParameter("json")).getOrElse("{}"))
                    }
                } catch {
                    case e : ParserUtil.ParseException =>
                        httpResponse.sendError(400, e.getMessage)
                        baseRequest.setHandled(true)
                        return
                    case MappingException(message, _) =>
                        httpResponse.sendError(400, message)
                        baseRequest.setHandled(true)
                        return
                }
                val request = Request(value, name => Option(httpRequest.getHeader(name)))
                val response = f(request)
                respond(httpResponse, response)
                baseRequest.setHandled(true)

            case None =>
        }
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

    def parseJson(manifest : Manifest[_], input : String) : Any = {
        Serialization.read(input)(formats, manifest)
    }

    def parseJson(manifest : Manifest[_], input : Reader) : Any = {
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
