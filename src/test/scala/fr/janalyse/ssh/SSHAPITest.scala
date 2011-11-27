/*
 * Copyright 2011 David Crosson
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.janalyse.ssh


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import SSH._
import scala.io.Source
import actors.Actor._
    
@RunWith(classOf[JUnitRunner])
class SSHAPITest extends FunSuite with ShouldMatchers {

  // ---------------------------------------------------------------------------
  test("One line exec with automatic resource close") {
    ssh(username="test") { _ execute "expr 1 + 1" }            should equal("2\n")
    ssh(username="test") { implicit ssh => "expr 1 + 1" !}     should equal("2\n")
    ssh(username="test") { _ execute "echo 1"::"echo 2"::Nil}  should equal("1\n"::"2\n"::Nil)
  }
  // ---------------------------------------------------------------------------
  test("Execution & file transferts within the same ssh session (autoclose)") {
    ssh(username="test") { implicit ssh =>
      val msg   = "/bin/echo -n 'Hello %s'".format(util.Properties.userName) !
      
      "HelloWorld.txt" put msg
      
      ("HelloWorld.txt" get) should equal(Some(msg))
      
      "HelloWorld.txt" >> "/tmp/sshtest.txt"
      
      Source.fromFile("/tmp/sshtest.txt").getLines().next() should equal(msg)
    }
  }
  // ---------------------------------------------------------------------------
  test("Bad performances obtained without persistent schell ssh channel (autoclose)") {
    ssh(username="test") { implicit ssh =>
      val remotedate = "date" !
      
      for(i <- 1 to 100) {"ls -d /tmp && echo 'done'" !}
    }
  }
  // ---------------------------------------------------------------------------
  test("Best performance is achieved with mutiple command within the same shell channel (autoclose)") {
    ssh(username="test") { _.shell { sh =>
      val remotedate = sh execute "date"
      for(i <- 1 to 100) {sh execute "ls -d /tmp && echo 'done'"}
      info("This one is at least 10 times faster than the previous one !")
      info("But now let's work on the syntax, because it is not simple enough")
    }}
  }
  // ---------------------------------------------------------------------------
  test("Start a remote process in background") {
    ssh(username="test") {implicit ssh =>
      val tester = self
      val receiver = actor {
        loop {
          react {
            case e:StandardErrorMessage  => info(e.line)
            case e:StandardOutputMessage => info(e.line)
          }
        }
      }
      val stdin = ssh.run("vmstat 1 10", receiver)
      Thread.sleep(10*1000L)
    }
  }

}

