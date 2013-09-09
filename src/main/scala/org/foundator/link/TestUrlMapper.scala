package org.foundator.link

import HttpMethod._
import HttpStatus._
import TestUrlHandlers._
import java.io.File

object TestUrlMapper extends UrlMapper {

    val rootUrl = url(getClass.getResource("/html/index.html"))
    val accountUrl = url(rootUrl, "account")
    val signInUrl = url(accountUrl, "sign-in", GET, signInHandler)
    val signOutUrl = url(accountUrl, "sign-out", POST, checkLogin(signOutHandler))
    val signUpUrl = url(accountUrl, "sign-up", GET, signUpHandler)

}

object TestUrlHandlers {

    def signInHandler(request : Request[{val count : Int}]) = {
        JsonResponse(OK, request.value.count)
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
        else StatusResponse(401)
    }
}

object Main {
    def main(args : Array[String]) {
        println(getClass.getResource("/html/index.html"))
        println(TestUrlMapper.rootUrl)
    }
}
