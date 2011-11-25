package fr.janalyse.ssh


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import SSH._
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class SSHAPITest extends FunSuite with ShouldMatchers {

  // ---------------------------------------------------------------------------
  test("One line exec with automatic resource close") {
    ssh(username="test") { _ execute "expr 1 + 1" }            should equal("2\n")
    ssh(username="test") { implicit ssh => "expr 1 + 1" !}     should equal("2\n")
    ssh(username="test") { _ execute "echo 1"::"echo 2"::Nil}  should equal("1\n"::"2\n"::Nil)
  }
  // ---------------------------------------------------------------------------
  test("Execution & file transferts within the same ssh session") {
    ssh(username="test") { implicit ssh =>
      val msg   = "/bin/echo -n 'Hello %s'".format(util.Properties.userName) !
      
      "HelloWorld.txt" put msg
      
      ("HelloWorld.txt" get) should equal(Some(msg))
      
      "HelloWorld.txt" >> "/tmp/sshtest.txt"
      
      Source.fromFile("/tmp/sshtest.txt").getLines().next() should equal(msg)
    }
  }
  // ---------------------------------------------------------------------------
  test("Bad performances obtained without persistent schell ssh channel") {
    ssh(username="test") { implicit ssh =>
      val remotedate = "date" !
      
      for(i <- 1 to 100) {"ls -d /tmp && echo 'done'" !}
    }
  }
  // ---------------------------------------------------------------------------
  test("Best performance is achieved with mutiple command within the same shell channel") {
    ssh(username="test") { _.shell {implicit sh =>
      val remotedate = sh execute "date"
      for(i <- 1 to 100) {sh execute "ls -d /tmp && echo 'done'"}
      info("This one is at least 10 times faster than the previous one !")
      info("But now let's work on the syntax, because it is not simple enough")
    }}
  }

}

