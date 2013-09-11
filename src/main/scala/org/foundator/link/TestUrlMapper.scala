package org.foundator.link

import HttpMethod._
import HttpStatus._
import TestUrlHandlers._
import java.io.File

object TestUrlMapper extends UrlMapper {

    val rootUrl = url(getClass.getResource("/html/index.html"))
    val staticUrl = url(rootUrl, "static", getClass.getResource("/html"))
    val accountUrl = url(rootUrl, "account")
    val signInUrl = url(accountUrl, "sign-in", GET, signInHandler)
    val signOutUrl = url(accountUrl, "sign-out", POST, checkLogin(signOutHandler))
    val signUpUrl = url(accountUrl, "sign-up", GET, signUpHandler)

}

object TestUrlHandlers {

    def signInHandler(request : Request[Count]) = {
        JsonResponse(OK, request.value.count + 1)
    }

    def signOutHandler(request : Request[(Int, String)]) = {
        JsonResponse(OK, "Hello")
    }

    def signUpHandler(request : Request[String]) : Response[Unit] = {
        StreamResponse.fromFile(OK, new File("test.txt"))
    }

    def checkLogin[I, O](handler : Request[(Int, I)] => Response[O])(request : Request[I]) : Response[O] = {
        val user = -1 // TODO: Check login
        if(user != -1) handler(request.copy(value = (user, request.value)))
        else StatusResponse(HttpStatus.NOT_FOUND)
    }
}

case class Count(count : Int)
