package org.foundator.link

import java.io.Reader
import java.io._
import java.net.{URLDecoder, InetSocketAddress, URI, URL}
import javax.servlet.MultipartConfigElement

import org.foundator.link.HttpStatus.OK
import org.json4s.{MappingException, ParserUtil}

import org.eclipse.jetty.server.handler.{RequestLogHandler, ResourceHandler, AbstractHandler}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.eclipse.jetty.server
import org.json4s.native.Serialization
import org.eclipse.jetty.util.resource.Resource
import org.eclipse.jetty.server.{NCSARequestLog, Server}
import org.json4s.ext.JodaTimeSerializers

abstract class UrlMapper {
    def run(port : Int, accessLogDirectory : Option[String] = None) {
        val server = new Server(port)
        server.setHandler(new UrlMapperHandler(this, accessLogDirectory))
        server.start()
        server.join()
    }

    def run(address : InetSocketAddress, accessLogDirectory : Option[String]) {
        val server = new Server(address)
        server.setHandler(new UrlMapperHandler(this, accessLogDirectory))
        server.start()
        server.join()
    }

    final def url() = addUniqueUrl(StrongUrl(None, None))
    final def url[I, O](method : HttpMethod, handler : Request[I] => Response[O])(implicit manifest : Manifest[I]) = addUniqueUrl(StrongUrl[I, O](None, Some((method, handler, manifest))))
    final def url[I, O](method : HttpMethod, handler : Request[I] => Response[O], subPaths : Boolean)(implicit manifest : Manifest[I]) = addUniqueUrl(StrongUrl[I, O](None, Some((method, handler, manifest)), if(subPaths) Some(null) else None))
    final def url(parentUrl : StrongUrl[_, _], path : String) = addUniqueUrl(StrongUrl(Some((parentUrl, path)), None))
    final def url[I, O](parentUrl : StrongUrl[_, _], path : String, method : HttpMethod, handler : Request[I] => Response[O])(implicit manifest : Manifest[I]) = addUniqueUrl(StrongUrl[I, O](Some((parentUrl, path)), Some((method, handler, manifest))))
    final def url[I, O](parentUrl : StrongUrl[_, _], path : String, method : HttpMethod, handler : Request[I] => Response[O], subPaths : Boolean)(implicit manifest : Manifest[I]) = addUniqueUrl(StrongUrl[I, O](Some((parentUrl, path)), Some((method, handler, manifest)), if(subPaths) Some(null) else None))
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

    // println(Paths.get(this.getClass.getResource("/test-to-be-deleted.txt").toURI).toFile)
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
            case StrongUrl(Some((parent, subPath)), _, _) => subPath :: pathOf(parent)
        }
        pathOf(this).reverse
    }

    val url = "/" + absolutePath.mkString("/")
}


case class Request[I](
    value : I,
    path : List[String],
    header : String => Option[String],
    parameter : String => Option[String],
    cookie : String => Option[String],
    rawRequest : server.Request
)

sealed abstract class Response[O] {def status : HttpStatus; def headers : List[(String, String)]}
case class StatusResponse[O](status : HttpStatus, headers : List[(String, String)] = List()) extends Response[O]
case class JsonResponse[O](status : HttpStatus, value : O, headers : List[(String, String)] = List()) extends Response[O]
case class StreamResponse[O](status : HttpStatus, writeToOutputStream : OutputStream => Unit, headers : List[(String, String)] = List()) extends Response[O]

object StreamResponse {
    def fromFile[O](status : HttpStatus, file : File, headers : List[(String, String)] = List()) : Response[O] =
        StreamResponse.fromInputStream[O](status, () => new BufferedInputStream(new FileInputStream(file)), headers)

    def fromInputStream[O](status : HttpStatus, inputStream : () => InputStream, headers : List[(String, String)] = List()) : Response[O] =
        StreamResponse[O](status, (out : OutputStream) => copyStream(inputStream(), out), headers)

    def fromFileStatusByFileExists[O](file : File, headers : List[(String, String)] = List()) : Response[O] = {
        val status = if(file.exists()) HttpStatus.OK else HttpStatus.NOT_FOUND
        fromFile(status, file, headers)
    }

    private def copyStream(input : InputStream, output : OutputStream) {
        val buffer = new Array[Byte](1024 * 4)
        while(true) {
            val bytesRead = input.read(buffer)
            if(bytesRead == -1) return
            output.write(buffer, 0, bytesRead)
        }
    }
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
    case object CREATED extends HttpStatus(201)
    case object ACCEPTED extends HttpStatus(202)
    case object NON_AUTHORITATIVE_INFORMATION extends HttpStatus(203)
    case object NO_CONTENT extends HttpStatus(204)
    case object RESET_CONTENT extends HttpStatus(205)
    case object PARTIAL_CONTENT extends HttpStatus(206)
    case object MULTIPLE_CHOISES extends HttpStatus(300)
    case object MOVED extends HttpStatus(301)
    case object FOUND extends HttpStatus(302)
    case object SEE_OTHER extends HttpStatus(303)
    case object NOT_MODIFIED extends HttpStatus(304)
    case object USE_PROXY extends HttpStatus(305)
    case object TEMPORARY_REDIRECT extends HttpStatus(307)
    case object BAD_REQUEST extends HttpStatus(400)
    case object UNAUTHORIZED extends HttpStatus(401)
    case object FORBIDDEN extends HttpStatus(403)
    case object NOT_FOUND extends HttpStatus(404)
    case object METHOD_NOT_ALLOWED extends HttpStatus(405)
    case object NOT_ACCEPTABLE extends HttpStatus(406)
    case object PROXY_AUTHENTICATION_REQUIRED extends HttpStatus(407)
    case object REQUEST_TIMEOUT extends HttpStatus(408)
    case object CONFLICT extends HttpStatus(409)
    case object GONE extends HttpStatus(410)
    case object LENGTH_REQUIRED extends HttpStatus(411)
    case object PRECONDITION_FAILED extends HttpStatus(412)
    case object REQUEST_ENTITY_TOO_LARGE extends HttpStatus(413)
    case object REQUEST_URI_TOO_LONG extends HttpStatus(414)
    case object UNSUPPORTED_MEDIA_TYPE extends HttpStatus(415)
    case object REQUESTED_RANGE_NOT_SATISFIABLE extends HttpStatus(416)
    case object EXPECTATION_FAILED extends HttpStatus(417)
    case object INTERNAL_SERVER_ERROR extends HttpStatus(500)
    case object NOT_IMPLEMENTED extends HttpStatus(501)
    case object BAD_GATEWAY extends HttpStatus(502)
    case object SERVICE_UNAVAILABLE extends HttpStatus(503)
    case object GATEWAY_TIMEOUT extends HttpStatus(504)
    case object HTTP_VERSION_NOT_SUPPORTED extends HttpStatus(505)
}


class UrlMapperHandler(urlMapper : UrlMapper, accessLogDirectory : Option[String] = None) extends AbstractHandler {

    val requestLogHandler = accessLogDirectory map { directory =>
        val requestLog = new NCSARequestLog()
        requestLog.setFilename(new File(directory, "/yyyy_mm_dd.request.log").getPath)
        requestLog.setFilenameDateFormat("yyyy_MM_dd")
        requestLog.setRetainDays(90)
        requestLog.setAppend(true)
        requestLog.setExtended(true)
        requestLog.setLogCookies(false)
        requestLog.setLogTimeZone("Europe/Copenhagen")
        val logHandler = new RequestLogHandler()
        logHandler.setRequestLog(requestLog)
        logHandler.start()
        logHandler
    }

    private val multiPartConfig = new MultipartConfigElement(System.getProperty("java.io.tmpdir"))

    def handle(target: String, baseRequest: server.Request, httpRequest: HttpServletRequest, httpResponse: HttpServletResponse) {

        // NOTE: Support for multi-part upload: http://dev.eclipse.org/mhonarc/lists/jetty-users/msg03294.html
        if (baseRequest.getContentType != null && baseRequest.getContentType.startsWith("multipart/form-data")) {
            baseRequest.setAttribute(server.Request.__MULTIPART_CONFIG_ELEMENT, multiPartConfig)
        }

        for(logHandler <- requestLogHandler) {
            logHandler.handle(target, baseRequest, httpRequest, httpResponse)
        }

        val path = if(httpRequest.getPathInfo != null) httpRequest.getPathInfo.split("/").filter(s => s.nonEmpty).toList else List()
        urlMapper.findUrl(httpRequest.getMethod, path) match {

            case Some((StrongUrl(_, None, Some(directory)), subPath)) =>
                if(directory != null) {
                    val resource = if(subPath.isEmpty) {
                        val path = directory.toString.reverse.dropWhile(_ != '/').tail.reverse
                        val file = directory.toString.reverse.takeWhile(_ != '/').reverse
                        baseRequest.setPathInfo("/" + (subPath :+ file).mkString("/"))
                        Resource.newResource(new URI(path))
                    } else {
                        baseRequest.setPathInfo("/" + subPath.mkString("/"))
                        Resource.newResource(directory)
                    }
                    val resourceHandler = new ResourceHandler()
                    resourceHandler.setBaseResource(resource)
                    resourceHandler.handle(target, baseRequest, httpRequest, httpResponse)
                } else {
                    System.err.println("404 The static directory serving '" + baseRequest.getPathInfo + "' was empty or non-existent when this server was started, and will be treated as empty until the server is restarted.")
                    httpResponse.sendError(404, "Not Found")
                    baseRequest.setHandled(true)
                }

            case Some((StrongUrl(_, Some((_, f : Function1[Request[_], Response[_]], manifest)), _), subPath)) =>
                val value = try {
                    if(httpRequest.getContentType != null && httpRequest.getContentType.matches("application/json([;].*)?")) {
                        parseJson(manifest, httpRequest.getReader)
                    } else if(httpRequest.getParameter("json") != null && httpRequest.getParameterMap.size() == 1) {
                        parseJson(manifest, httpRequest.getParameter("json"))
                    } else {
                        parseJson(manifest, "{}")
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
                def cookie(name : String) : Option[String] = httpRequest.getCookies.collectFirst {
                    case c if c.getName == name => URLDecoder.decode(c.getValue, Option(httpRequest.getCharacterEncoding).getOrElse("UTF-8"))
                }
                val request = Request(value, subPath, name => Option(httpRequest.getHeader(name)), name => Option(httpRequest.getParameter(name)), cookie, baseRequest)
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
            httpResponse.setContentType("application/json")
            httpResponse.setCharacterEncoding("UTF-8")
            updateResponse(httpResponse, status, headers)
            Serialization.write(value, httpResponse.getWriter)(formats)
        case StreamResponse(status, writeToOutputStream, headers) =>
            updateResponse(httpResponse, status, headers)
            writeToOutputStream(httpResponse.getOutputStream)
    }

    def updateResponse(response: HttpServletResponse, status : HttpStatus, headers : List[(String, String)]) {
        response.setStatus(status.code)
        for((name, value) <- cacheHeaders(headers)) {
            response.addHeader(name, value)
        }
    }

    def cacheHeaders(headers : List[(String, String)]) = {
        // Internet Explorer caches AJAX GET requests unless it's explicitly prohibited
        // The Expires: -1 header ensures that it doesn't, except when offline
        val cache = List("Cache-Control", "Expires", "ETag").exists { header => headers.exists(_._1.toLowerCase == header.toLowerCase) }
        if(cache) headers else ("Expires" -> "-1") :: headers
    }

    def parseJson(manifest : Manifest[_], input : String) : Any = {
        Serialization.read(input)(formats, manifest)
    }

    def parseJson(manifest : Manifest[_], input : Reader) : Any = {
        Serialization.read(input)(formats, manifest)
    }

    import org.json4s._
    private val formats = DefaultFormats + FieldSerializer[Object]() ++ JodaTimeSerializers.all
}
