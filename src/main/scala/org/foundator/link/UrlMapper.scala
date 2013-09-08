package org.foundator.link

import java.io.{FileInputStream, BufferedInputStream, InputStream, File}
import HttpMethod._

// TODO: Mechanism for serving directories, eg. url(someUrl, "static", new File("../assets"))
class UrlMapper {
    final def url() = addUniqueUrl(StrongUrl(None, None))
    final def url[I, O](method : HttpMethod, handler : Request[I] => Response[O]) = addUniqueUrl(StrongUrl[I, O](None, Some((method, handler))))
    final def url(parentUrl : StrongUrl[_, _], path : String) = addUniqueUrl(StrongUrl(Some((parentUrl, path)), None))
    final def url[I, O](parentUrl : StrongUrl[_, _], path : String, method : HttpMethod, handler : Request[I] => Response[O]) = addUniqueUrl(StrongUrl[I, O](Some((parentUrl, path)), Some((method, handler))))
    final def url(parentUrl : StrongUrl[_, _], path : String, file : File) = // How poor is Scala's type inference?
        addUniqueUrl(StrongUrl(
            Some((parentUrl, path)),
            Some((GET, (r : Request[File]) => handleDirectory(file, r))), directory = true))

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

    private def handleDirectory(baseFile : File, request : Request[File]) : Response[Unit] = {
        val file = request.value
        if(file.isAbsolute) StatusResponse(404)
        else if(file.toString.startsWith(".." + File.separator)) StatusResponse(404)
        else if(file.toString.contains(File.separator + ".." + File.separator)) StatusResponse(404)
        else {
            val result = new File(baseFile, file.toString)
            if(!file.isFile) StatusResponse(404)
            else if(!file.exists) StatusResponse(404)
            else StreamResponse.fromFile(200, result)
        }
    }
}


case class DuplicateUrlException(path : List[String], method : Option[HttpMethod]) extends RuntimeException("Path: " + path + ", method: " + method)


case class StrongUrl[I, O](path : Option[(StrongUrl[_, _], String)], handler : Option[(HttpMethod, Request[I] => Response[O])], directory : Boolean = false)


trait Request[I] {
    def value : I
    def header(name : String) : Option[String]
}

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