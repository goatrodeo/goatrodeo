package org.stambecco
package harness

import net.liftweb._
import http._
import util.Helpers._

import testing._
import json._
import JsonAST._
import JsonDSL._
// import Printer._

object Run extends TestKit {
  def baseUrl = "http://localhost:8080"
  
  def addUser(who: String = "Archer") {
    val obj = ("name" -> who)
      
      val str = compact(render(obj))
    
    
    println(post("/user", str.getBytes("UTF-8"), "text/json") match {
      case r: HttpResponse => for { body <- r.body } { new String(body, "UTF-8") + " "+ r.code }
    })
  }

  def sendMessage(to: Long) {
    val obj = ("messageId" -> randomLong(100000000)) ~ ("timestamp" -> millis)

    val str = compact(render(obj))

    println(put("/message/"+to, str.getBytes("UTF-8"), "text/json") match {
      case r: HttpResponse => for { body <- r.body } { new String(body, "UTF-8") + " "+ r.code }
    })
  }

  def follow(from: Long, who: Long) {
    val obj = ("who" -> who)

    val str = compact(render(obj))

    println(put("/follow/"+from, str.getBytes("UTF-8"), "text/json") match {
      case r: HttpResponse => for { body <- r.body } { new String(body, "UTF-8") + " "+ r.code }
    })
  }

  def main(argv: Array[String]) {
    argv.toList match {
      case "-m" :: AsLong(id) :: Nil => sendMessage(id)

      case "-f" :: AsLong(from) :: AsLong(to) :: Nil =>
        follow(from, to)

      case Nil => addUser()
      case x :: Nil => addUser(x)

      case x => println("Didn't understand "+x.mkString(" "))
    }
  }
}

