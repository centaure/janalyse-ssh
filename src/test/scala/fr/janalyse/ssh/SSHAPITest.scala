package fr.janalyse.ssh


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import SSH._

//import scala.collection.JavaConversions._

class SSHAPITest extends FunSuite with ShouldMatchers {
  /*
  // ---------------------------------------------------------------------------
  test("Basic tests") {
    using(SSHSession(host="localhost", username="test", password="testtest")) { implicit session =>
      val hostname = "hostname".execute
      List("echo 1", "echo 2", "echo 3").execute.map(_.trim) should equal (List("1", "2", "3"))
      "Hello World" put "file.tst"
      "file.tst".get.getOrElse("") should equal ("Hello World")
    }
  }
  // ---------------------------------------------------------------------------
  test("Basic tests revisited") {
    using("mylocal:127.0.0.1:test:testtest") { implicit session =>
      val hostname = "hostname".execute
      List("echo 1", "echo 2", "echo 3").execute.map(_.trim) should equal (List("1", "2", "3"))
      "Hello World" put "file.tst"
      "file.tst".get.getOrElse("") should equal ("Hello World")
    }
  }
  */
  // ---------------------------------------------------------------------------
  test("NEW-1") {
    val files = ssh(username="test") { _ execute "ls" }
    info("***"+files.trim+"***")
  }
  // ---------------------------------------------------------------------------
  test("NEW-2") {
    ssh(username="test") { implicit ssh =>
      val uname   = "uname"!
      
      //"HelloWorld" >> "hello.txt"
      
      val content = "hello.txt".get
      
      val List(uname2, hostname2, id2) = List("uname -a","hostname","id") !
      
      info(content.toString)
      info(uname2)
    }
  }
  // ---------------------------------------------------------------------------

}

