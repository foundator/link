link
====

Strongly typed URLs with dispatch &amp; handlers

WORK IN PROGRESS
----------------

This library is not yet in a usable state.


Example code
------------

```scala
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
```


The above code will define the following urls:

    /about                          -> class path resource /html/about.html
    /js/<whatever>                  -> class path resource /js/<whatever>
    /css/<whatever>                 -> class path resource /css/<whatever>
    /add?json={"x": <x>, "y": <y>}  -> Handlers.add(Addition(<x>, <y>))

The /add url will only handle GET requests, since that was the specified HTTP method.
If the json is missing or can't be deserialized, it will report 400 Bad Request.

Development mode
----------------

If you're using Maven, your directory structure could look like:

    /pom.xml                -> Maven project file
    /src/main/resources/    -> Your static assets like CSS, images, etc.
    /src/main/scala/        -> The above example code and other scala code.

If you do follow this standard Maven convention, 
then this library will figure out when you're running in development mode, based 
on the pom.xml file existing and that the resource URLs has the `file` protocol.

This means that a resource in /target/classes/{whatever} will instead be fetched
from /src/main/resources/{whatever}, thus giving you instant updates for 
everything in the resources directory. 

You only need to restart the server when
you need to compile code (eg. make edits in the scala/ directory).
Since we're using Jetty, a restart takes about 2 seconds plus of course the time
it takes to compile the changed code.
