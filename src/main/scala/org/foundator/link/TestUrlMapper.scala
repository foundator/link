package org.foundator.link

import HttpMethod._
import HttpStatus._
import TestUrlHandlers._
import java.io.File

object TestUrlMapper extends UrlMapper {

    val rootUrl = url()
    val staticUrl = url(rootUrl, "static", new File("/home/tomcat7/webapp/dont/do/this"))
    val accountUrl = url(rootUrl, "account")
    val signInUrl = url(accountUrl, "sign-in", GET, signInHandler)
    val signOutUrl = url(accountUrl, "sign-out", POST, signOutHandler)
    val signUpUrl = url(accountUrl, "sign-up", GET, signUpHandler)

}

object TestUrlHandlers {

    def signInHandler(request : Request[{val count : Int}]) = {
        JsonResponse(OK, request.value.count)
    }

    def signOutHandler(request : Request[String]) = {
        JsonResponse(OK, "Hello")
    }

    def signUpHandler(request : Request[String]) : Response[Unit] = {
        StreamResponse.fromFile(OK, new File("test.txt"))
    }
}
