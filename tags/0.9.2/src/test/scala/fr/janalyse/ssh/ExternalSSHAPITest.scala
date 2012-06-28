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

  test("Hello 1") {
    jassh.SSH.once(sshopts) { _.executeAndTrim("echo 'hello'") } should equal("hello")
  }

  test("Hello 2") {
    jassh.SSH.shell(sshopts) { _.executeAndTrim("echo 'hello'") } should equal("hello")
  }

  test("Hello 3") {
    jassh.SSH.shell(host, user, password=pass) { _.executeAndTrim("echo 'hello'") } should equal("hello")
  }
  
}
