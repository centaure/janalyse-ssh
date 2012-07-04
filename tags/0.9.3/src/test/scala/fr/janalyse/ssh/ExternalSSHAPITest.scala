package fr.janalyse.ssh.external

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExternalSSHAPITest extends FunSuite with ShouldMatchers {
  
  val host="127.0.0.1"
  val user="test"
  val pass="testtest"
    
  val sshopts = jassh.SSHOptions(host, user, password = pass)

  // -------------------------------------------------------------------
  // -- With a global import
  {
	  import jassh._
	
	  test("Hello 1") {
	    SSH.once(sshopts) { _.executeAndTrim("echo 'hello'") } should equal("hello")
	  }
	
	  test("Hello 2") {
	    SSH.shell(sshopts) { _.executeAndTrim("echo 'hello'") } should equal("hello")
	  }
	
	  test("Hello 3") {
	    SSH.shell(host, user, password=pass) { _.executeAndTrim("echo 'hello'") } should equal("hello")
	  }
  }

  
  // -------------------------------------------------------------------
  // -- Without any jassh imports
  {
    test("Hello 4") {
      fr.janalyse.ssh.SSH.once(sshopts) { _.executeAllAndTrim(List("echo 'hello'")) } should equal(List("hello"))
    }
    test("Hello 5") {
      jassh.SSH.once(sshopts) { _.executeAllAndTrim(List("echo 'hello'")) } should equal(List("hello"))
    }
  }
  
  
}



