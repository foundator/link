link
====

Strongly typed URLs with dispatch &amp; handlers

WORK IN PROGRESS
----------------

This library is not yet in a usable state.


Example code
------------

    import org.foundator.link._
    import org.foundator.link.HttpMethod._
    import org.foundator.link.HttpStatus._
    
    object Urls extends UrlMapper {
        
        val rootUrl = url()
        val aboutUrl = url(rootUrl, "about", resource("/html/about.html"))
        val jsUrl = url(rootUrl, "js", resource("/js"))
        val cssUrl = url(rootUrl, "css", resource("/css"))
        val addUrl = url(rootUrl, "add", GET, Handlers.add)
    
    }
    
    object Handlers {
        def add(request : Request[Addition]) : Response[Int] = {
            JsonResponse(OK, request.value.x + request.value.y)
        }
        
        case class Addition(x : Int, y : Int)
    }
    
    object Main {
        def main(arguments : Array[String]) {
            Urls.run(8080)
        }
    }
