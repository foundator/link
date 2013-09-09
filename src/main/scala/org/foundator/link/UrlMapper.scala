package org.foundator.link

import java.io._
import HttpMethod._
import java.net.{URI, URL}
import scala.Some

abstract class UrlMapper {
    final def url() = addUniqueUrl(StrongUrl(None, None))
    final def url[I, O](method : HttpMethod, handler : Request[I] => Response[O]) = addUniqueUrl(StrongUrl[I, O](None, Some((method, handler))))
    final def url(parentUrl : StrongUrl[_, _], path : String) = addUniqueUrl(StrongUrl(Some((parentUrl, path)), None))
    final def url[I, O](parentUrl : StrongUrl[_, _], path : String, method : HttpMethod, handler : Request[I] => Response[O]) = addUniqueUrl(StrongUrl[I, O](Some((parentUrl, path)), Some((method, handler))))

    final def url(base : URL) : StrongUrl[String, Unit] = url(base.toURI)
    final def url(base : URI) : StrongUrl[String, Unit] =
        addUniqueUrl(StrongUrl(
            None,
            Some((GET, (r : Request[String]) => handleDirectory(base, r))), directory = true))
    final def url(parentUrl : StrongUrl[_, _], path : String, base : URL) : StrongUrl[String, Unit] = url(parentUrl, path, base.toURI)
    final def url(parentUrl : StrongUrl[_, _], path : String, base : URI) : StrongUrl[String, Unit] =
        addUniqueUrl(StrongUrl(
            Some((parentUrl, path)),
            Some((GET, (r : Request[String]) => handleDirectory(base, r))), directory = true))


    // TODO: Look for a way to avoid side effects for constructing the list of URLs
    private def addUniqueUrl[I, O](url : StrongUrl[I, O]) : StrongUrl[I, O] = {
        val path = getPath(url)
        val method = url.handler.map(_._1)
        if(urls((path, method))) throw DuplicateUrlException(path, method)
        urls += ((path, method))
        url
    }
    private def getPath(url : StrongUrl[_, _]) = {
        def pathOf(url : StrongUrl[_, _]) : List[String] = url match {
            case StrongUrl(None, _, _) => List()
            case StrongUrl(Some((parent, path)), _, _) => path :: pathOf(parent)
        }
        pathOf(url).reverse
    }
    private var urls = Set[(List[String], Option[HttpMethod])]()

    private def handleDirectory(base : URI, request : Request[String]) : Response[Unit] = {
        val file = if(request.value.isEmpty || request.value.startsWith("/")) request.value else "/" + request.value
        if(file.toString.contains("/../")) StatusResponse(404)
        else {
            try {
                val result = base.resolve(file).toURL
                StreamResponse(200, () => result.openStream())
            } catch {
                case e : IOException => StatusResponse(404)
            }
        }
    }
}


case class DuplicateUrlException(path : List[String], method : Option[HttpMethod]) extends RuntimeException("Path: " + path + ", method: " + method)


case class StrongUrl[I, O](path : Option[(StrongUrl[_, _], String)], handler : Option[(HttpMethod, Request[I] => Response[O])], directory : Boolean = false)


case class Request[I](value : I, header : String => Option[String])

sealed abstract class Response[O] {def status : Int; def headers : List[(String, String)]}
case class StatusResponse[O](status : Int, headers : List[(String, String)] = List()) extends Response[O]
case class JsonResponse[O](status : Int, value : O, headers : List[(String, String)] = List()) extends Response[O]
case class StreamResponse[O](status : Int, inputStream : () => InputStream, headers : List[(String, String)] = List()) extends Response[O]

object StreamResponse {
    def fromFile[O](status : Int, file : File, headers : Map[String, String] = Map(), cookies : Map[String, String] = Map()) =
        StreamResponse[O](status, () => new BufferedInputStream(new FileInputStream(file)))
}


sealed abstract class HttpMethod

object HttpMethod {
    case object GET extends HttpMethod
    case object PUT extends HttpMethod
    case object POST extends HttpMethod
    case object DELETE extends HttpMethod
    case object HEAD extends HttpMethod
}


object HttpStatus {
    val CONTINUE = 100
    val SWITCHING_PROTOCOLS = 101
    val OK = 200
    val INTERNAL_SERVER_ERROR = 500
    // TODO: All standard status codes
}