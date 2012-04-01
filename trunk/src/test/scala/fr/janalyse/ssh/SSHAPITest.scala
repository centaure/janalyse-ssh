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
import scala.io.Source
import actors.Actor._
import scala.util.Properties
import java.io.File

@RunWith(classOf[JUnitRunner])
class SSHAPITest extends FunSuite with ShouldMatchers {

  def howLongFor[T](what: () => T) = {
    val begin = System.currentTimeMillis()
    val result = what()
    val end = System.currentTimeMillis()
    (end - begin, result)
  }

  //==========================================================================================================
  test("One line exec with automatic resource close") {
    SSH(username = "test") { _ execute "expr 1 + 1" trim } should equal("2")
    SSH(username = "test") { _ executeAndTrim "expr 1 + 1" } should equal("2")
    SSH(username = "test") { _ executeAndTrim "echo 1" :: "echo 2" :: Nil } should equal("1" :: "2" :: Nil)
    val year = SSH(username = "test") { _ executeAndTrim "expr 1 + 10" toInt }
    year should equal(11)
  }
  //==========================================================================================================
  test("Execution & file transferts within the same ssh session (autoclose)") {
    SSH(username = "test") { ssh =>
      val msg = ssh execute "/bin/echo -n 'Hello %s'".format(util.Properties.userName)

      ssh.put(msg, "HelloWorld.txt")

      (ssh get "HelloWorld.txt") should equal(Some(msg))

      ssh.receive("HelloWorld.txt", "/tmp/sshtest.txt")

      Source.fromFile("/tmp/sshtest.txt").getLines().next() should equal(msg)
    }
  }
  
  //==========================================================================================================
  test("shell coherency check") {
    SSH.shell(username="test") { sh=>
	  (1 to 100) foreach {i =>
	  sh.executeAndTrim("echo ta"+i) should equal("ta"+i)
	  sh.executeAndTrim("echo ga"+i) should equal("ga"+i)
	  }
    }
  }

  //==========================================================================================================
  test("shell coherency check with long command lines (in //)") {
    SSH(username="test") { ssh=>
	  (1 to 20).par foreach {i =>
	    ssh.shell {sh=>
	        def mkmsg(base:String) = base*100+i
		    sh.executeAndTrim("echo %s".format(mkmsg("Z"))) should equal(mkmsg("Z"))
		    sh.executeAndTrim("echo %s".format(mkmsg("ga"))) should equal(mkmsg("ga"))
		    sh.executeAndTrim("echo %s".format(mkmsg("PXY"))) should equal(mkmsg("PXY"))
	        sh.executeAndTrim("echo %s".format(mkmsg("GLoups"))) should equal(mkmsg("GLoups"))
	    }
	  }
    }
  }
  //==========================================================================================================
  test("SSHShell : Bad performances obtained without persistent schell ssh channel (autoclose)") {
    val howmany=200
    SSH(username = "test") { ssh =>
      val (dur, _) = howLongFor(() =>
        for (i <- 1 to howmany) { ssh execute "ls -d /tmp && echo 'done'" })
      val throughput = howmany.doubleValue() / dur * 1000
      info("Performance using shell without channel persistency : %.1f cmd/s".format(throughput))
    }
  }
  //==========================================================================================================
  test("SSHShell : Best performance is achieved with mutiple command within the same shell channel (autoclose)") {
    val howmany=1000
    SSH(username = "test") {
      _.shell { sh =>
        val (dur, _) = howLongFor(() =>
          for (i <- 1 to howmany) { sh execute "ls -d /tmp && echo 'done'" })
        val throughput = howmany.doubleValue() / dur * 1000
        info("Performance using with channel persistency : %.1f cmd/s".format(throughput))
      }
    }
  }
  //==========================================================================================================
  test("SSHExec : performances obtained using exec ssh channel (no persistency)") {
    val howmany=200
    SSH(username = "test") { ssh =>
      val (dur, _) = howLongFor(() =>
        for (i <- 1 to howmany) { ssh execOnce "ls -d /tmp && echo 'done'"})
      val throughput = howmany.doubleValue() / dur * 1000
      info("Performance using exec ssh channel (no persistency) : %.1f cmd/s".format(throughput))
    }
  }
  //==========================================================================================================
  test("Start a remote process in background") {
    import fr.janalyse.ssh.SSH
    SSH(username = "test") { ssh =>
      
      var x=List.empty[String]
      
      def receiver(data:Option[String]) {data foreach {d => x = x :+ d} }
      val executor = ssh.run("vmstat 1 5", receiver)

      executor.waitForEnd

      x.zipWithIndex map { case (l, i) => info("%d : %s".format(i, l)) }
      x.size should equal(7)      
    }
  }

  //==========================================================================================================
  test("Usage case example - for tutorial") {
    import fr.janalyse.ssh.SSH
    SSH(host = "localhost", username = "test") { ssh =>
      
      val uname = ssh executeAndTrim "uname -a"
      val fsstatus = ssh execute "df -m"
      val fmax = ssh get "/proc/sys/fs/file-max"
      
      ssh.shell { sh => // For higher performances
        val hostname = sh.executeAndTrim("hostname")
        val files = sh.execute("find /usr/lib/")
      }
      ssh.ftp { ftp => // For higher performances
        val cpuinfo = ftp.get("/proc/cpuinfo")
        val meminfo = ftp.get("/proc/meminfo")
      }
      // output streaming
      def receiver(data:Option[String]) {data foreach {println(_)}}
      val executor = ssh.run("vmstat 1 3", receiver)
      executor.waitForEnd
    }
  }
  
  test("Simultaenous SSH operations") {
    val started = System.currentTimeMillis()
    val h="127.0.0.1"
    val cnxinfos = List(h,h,h,h,h) map {h=> SSHOptions(host=h, username="test")}
    val sshs   = cnxinfos.par map {SSH(_)}
    val unames = sshs map {_ execute "date; sleep 5"}
    info(unames.mkString("----"))
    
    (System.currentTimeMillis() - started) should be < (6000L)  //(and not 5s * 5 = 25s)
  }

  test("Simplified persistent ssh shell usage") {
    SSH.shell(host = "localhost", username = "test") { sh =>
      sh.execute("ls")
      sh.execute("uname")
    }
  }

  test("Simplified persistent ssh shell and ftp usage") {
    SSH.shellAndFtp(host = "localhost", username = "test") { (sh, ftp) =>
      sh.execute("ls")
      sh.execute("uname")
      ftp.get("/proc/stat")
      ftp.get("/proc/vmstat")
    }
  }
  
  test("simplified usage with sshOptions as Option") {
    val cnxinfo = Some(SSHOptions(host="localhost", username="test"))
    val stat = SSH.ftp(cnxinfo) {_.get("/proc/stat")}
    
    stat should not equal(None)
    stat.get.size should be >(0)
  }
}

