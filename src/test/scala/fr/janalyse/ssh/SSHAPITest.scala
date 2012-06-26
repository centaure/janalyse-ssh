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
import java.io.IOException

@RunWith(classOf[JUnitRunner])
class SSHAPITest extends FunSuite with ShouldMatchers {

  def howLongFor[T](what: => T) = {
    val begin = System.currentTimeMillis()
    val result = what
    val end = System.currentTimeMillis()
    (end - begin, result)
  }

  val sshopts = SSHOptions("127.0.0.1", "test", password="testtest")
  //val sshopts = SSHOptions("192.168.2.238", "test", password=Some("testtest"), port=22022)
  //val sshopts = SSHOptions("www.janalyse.fr")
  
  //==========================================================================================================
  test("One line exec with automatic resource close") {
    SSH.once(sshopts) { _ execute "expr 1 + 1" trim } should equal("2")
    SSH.once(sshopts) { _ executeAndTrim "expr 1 + 1" } should equal("2")
    SSH.once(sshopts) { _ executeAndTrim "echo 1" :: "echo 2" :: Nil } should equal("1" :: "2" :: Nil)
    val year = SSH.once(sshopts) { _ executeAndTrim "expr 1 + 10" toInt }
    year should equal(11)
  }
  //==========================================================================================================
  test("Execution & file transferts within the same ssh session (autoclose)") {
    SSH.once(sshopts) { ssh =>
      val msg = ssh execute "/bin/echo -n 'Hello %s'".format(util.Properties.userName)

      ssh.put(msg, "HelloWorld.txt")

      (ssh get "HelloWorld.txt") should equal(Some(msg))

      ssh.receive("HelloWorld.txt", "/tmp/sshtest.txt")

      Source.fromFile("/tmp/sshtest.txt").getLines().next() should equal(msg)
    }
  }
  
  //==========================================================================================================
  test("shell coherency check") {
    SSH.shell(sshopts) { sh=>
	  (1 to 100) foreach {i =>
	  sh.executeAndTrim("echo ta"+i) should equal("ta"+i)
	  sh.executeAndTrim("echo ga"+i) should equal("ga"+i)
	  }
    }
  }

  //==========================================================================================================
  test("shell coherency check with long command lines (in //)") {
    SSH.once(sshopts) { ssh=>
	  (1 to 10).par foreach {i =>
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
    SSH.once(sshopts) { ssh =>
      val (dur, _) = howLongFor {
        for (i <- 1 to howmany) { ssh execute "ls -d /tmp && echo 'done'" }
      }
      val throughput = howmany.doubleValue() / dur * 1000
      info("Performance using shell without channel persistency : %.1f cmd/s".format(throughput))
    }
  }
  //==========================================================================================================
  test("SSHShell : Best performance is achieved with mutiple command within the same shell channel (autoclose)") {
    val howmany=5000
    SSH.once(sshopts) {
      _.shell { sh =>
        val (dur, _) = howLongFor {
          for (i <- 1 to howmany) { sh execute "ls -d /tmp && echo 'done'" }
        }
        val throughput = howmany.doubleValue() / dur * 1000
        info("Performance using with channel persistency : %.1f cmd/s".format(throughput))
      }
    }
  }
  //==========================================================================================================
  test("SSHExec : performances obtained using exec ssh channel (no persistency)") {
    val howmany=200
    SSH.once(sshopts) { ssh =>
      val (dur, _) = howLongFor {
        for (i <- 1 to howmany) { ssh execOnce "ls -d /tmp && echo 'done'"}
      }
      val throughput = howmany.doubleValue() / dur * 1000
      info("Performance using exec ssh channel (no persistency) : %.1f cmd/s".format(throughput))
    }
  }
  //==========================================================================================================
  test("Start a remote process in background") {
    import fr.janalyse.ssh.SSH
    SSH.once(sshopts) { ssh =>
      
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
    SSH.once(sshopts) { ssh =>
      
      val uname = ssh executeAndTrim "uname -a"
      val fsstatus = ssh execute "df -m"
      val fmax = ssh get "/proc/sys/fs/file-max" // Warning SCP only work with regular file
      
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
  
  //==========================================================================================================
  test("Simultaenous SSH operations") {    
    val started = System.currentTimeMillis()
    val cnxinfos = List(sshopts, sshopts, sshopts, sshopts, sshopts)
    val sshs   = cnxinfos.par map {SSH(_)}
    
    // Configuring parallelism
    // Scala 2.9
    collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(6)
    // Scala 2.10
    //sshs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(6))
    
    val unames = sshs map {_ execute "date; sleep 5"}
    info(unames.mkString("----"))
    
    (System.currentTimeMillis() - started) should be < (8000L)  //(and not 5s * 5 = 25s)
  }

  //==========================================================================================================
  test("Simplified persistent ssh shell usage") {
    SSH.shell("localhost", "test") { sh =>
      sh.execute("ls")
      sh.execute("uname")
    }
  }

  //==========================================================================================================
  test("Simplified persistent ssh shell and ftp usage") {
    SSH.shellAndFtp(sshopts) { (sh, ftp) =>
      sh.execute("ls")
      sh.execute("uname")
      ftp.get("/proc/stat")
      ftp.get("/proc/vmstat")
    }
  }
  
  //==========================================================================================================
  test("simplified usage with sshOptions as Option") {
    val cnxinfo = Some(sshopts)
    val stat = SSH.ftp(cnxinfo) {_.get("/proc/stat")}
    
    stat should not equal(None)
    stat.get.size should be >(0)
  }
  //==========================================================================================================
  ignore("timeout tests") { // TODO : not working, make timeout possible with too long running remote command; (^C is already possible)!!
    val opts = SSHOptions(host="localhost", username="test", timeout=5000, connectTimeout=2000)
    SSH.once(opts) {ssh =>
      ssh.executeAndTrim("sleep 4; echo 'ok'") should equal("ok")
      intercept[IOException] {
        ssh.executeAndTrim("sleep 10; echo 'ok'") should equal("ok")
      }
    }
  }
  //==========================================================================================================
  test("helper methods") {
    val testfile="sshapitest.dummy"
    val testdir="sshapitest-dummydir"
    def now = new java.util.Date()
    val started = now
    SSH.shell("localhost", "test") {sh =>
      
      // create a dummy file and dummy directory
      sh.execute("echo -n 'toto' > %s".format(testfile))
      sh.execute("mkdir -p %s".format(testdir))
      val homedir = sh.executeAndTrim("pwd")
      val rhostname = sh.executeAndTrim("hostname")
      
      // now tests the utilities methods
      import sh._
      uname                   should equal("Linux")
      hostname                should equal(rhostname)
      fileSize(testfile)      should equal(Some(4))
      md5sum(testfile)        should equal(Some("f71dbe52628a3f83a77ab494817525c6"))
      md5sum(testfile)        should equal(Some(SSHTools.md5sum("toto")))
      sha1sum(testfile)       should equal(Some("0b9c2625dc21ef05f6ad4ddf47c5f203837aa32c"))
      ls                      should contain(testfile)
      cd(testdir)
      pwd                     should equal(homedir+"/"+testdir)
      cd()
      pwd                     should equal(homedir)
      sh.test("1 == 1")       should equal(true)
      sh.test("1 == 2")       should equal(false)
      isFile(testfile)        should equal(true)
      isDirectory(testfile)   should equal(false)
      exists(testfile)        should equal(true)
      isExecutable(testfile)  should equal(false)
      findAfterDate(".", started) should have size(1)
      val reftime = now.getTime
      date().getTime          should (be>(reftime-1000) and be<(reftime+1000))
    }
  }

  //==========================================================================================================
  ignore("file transfert performances (with content loaded in memory, no subchannel reused)") {
    val testfile="test-transfert"
      
    def withSCP(filename:String, ssh:SSH, howmany:Int, sizeKb:Int) {
      for(_ <- 1 to howmany)
        ssh.getBytes(filename).map(_.length) should equal(Some(sizeKb*1024))
    }
    def withSFTP(filename:String, ssh:SSH, howmany:Int, sizeKb:Int) {
      for(_ <- 1 to howmany)
        ssh.ftp(_.getBytes(filename)).map(_.length) should equal(Some(sizeKb*1024))
    }
    def withReusedSFTP(filename:String, ssh:SSH, howmany:Int, sizeKb:Int) {
      ssh.ftp { ftp =>
        for(_ <- 1 to howmany)
          ftp.getBytes(filename).map(_.length) should equal(Some(sizeKb*1024))
      }
    }
      
    
    def toTest(thattest:(String, SSH, Int, Int)=>Unit,
               howmany:Int,
               sizeKb:Int,
               comments:String)(ssh:SSH) {
      ssh.execute("dd count=%d bs=1024 if=/dev/zero of=%s".format(sizeKb, testfile))
      val (d, _) = howLongFor {
        thattest(testfile, ssh, howmany, sizeKb) 
      }
      info("Bytes rate : %.1fMb/s %dMb in %.1fs for %d files - %s".format(howmany*sizeKb*1000L/d/1024d, sizeKb*howmany/1024, d/1000d, howmany, comments))      
    }
    
    val withCipher=SSHOptions("localhost", "test", noneCipher=false)
    val noneCipher=SSHOptions("localhost", "test", noneCipher=true)
    
    SSH.once(withCipher) (toTest(withSCP, 5, 100*1024, "byterates using SCP"))
    SSH.once(noneCipher) (toTest(withSCP, 5, 100*1024, "byterates using SCP (with none cipher)"))
    SSH.once(withCipher) (toTest(withSFTP, 5, 100*1024, "byterates using SFTP"))
    SSH.once(noneCipher) (toTest(withSFTP, 5, 100*1024, "byterates using SFTP (with none cipher)"))
    SSH.once(withCipher) (toTest(withReusedSFTP, 5, 100*1024, "byterates using SFTP (session reused"))
    SSH.once(noneCipher) (toTest(withReusedSFTP, 5, 100*1024, "byterates using SFTP (session reused, with none cipher)"))
    
    SSH.once(withCipher) (toTest(withSCP, 500, 1024, "byterates using SCP"))
    SSH.once(noneCipher) (toTest(withSCP, 500, 1024, "byterates using SCP (with none cipher)"))
    SSH.once(withCipher) (toTest(withSFTP, 500, 1024, "byterates using SFTP"))
    SSH.once(noneCipher) (toTest(withSFTP, 500, 1024, "byterates using SFTP (with none cipher)"))
    SSH.once(withCipher) (toTest(withReusedSFTP, 500, 1024, "byterates using SFTP (session reused)"))
    SSH.once(noneCipher) (toTest(withReusedSFTP, 500, 1024, "byterates using SFTP (session reused, with none cipher)"))
  }

  //==========================================================================================================
  test("ssh compression") {
    val testfile="test-transfert"
      
    def withSCP(filename:String, ssh:SSH, howmany:Int, sizeKb:Int) {
      for(_ <- 1 to howmany)
        ssh.getBytes(filename).map(_.length) should equal(Some(sizeKb*1024))
    }
    def withSFTP(filename:String, ssh:SSH, howmany:Int, sizeKb:Int) {
      for(_ <- 1 to howmany)
        ssh.ftp(_.getBytes(filename)).map(_.length) should equal(Some(sizeKb*1024))
    }
    def withReusedSFTP(filename:String, ssh:SSH, howmany:Int, sizeKb:Int) {
      ssh.ftp { ftp =>
        for(_ <- 1 to howmany)
          ftp.getBytes(filename).map(_.length) should equal(Some(sizeKb*1024))
      }
    }
      
    
    def toTest(thattest:(String, SSH, Int, Int)=>Unit,
               howmany:Int,
               sizeKb:Int,
               comments:String)(ssh:SSH) {
      ssh.execute("dd count=%d bs=1024 if=/dev/zero of=%s".format(sizeKb, testfile))
      val (d, _) = howLongFor {
        thattest(testfile, ssh, howmany, sizeKb) 
      }
      info("Bytes rate : %.1fMb/s %dMb in %.1fs for %d files - %s".format(howmany*sizeKb*1000L/d/1024d, sizeKb*howmany/1024, d/1000d, howmany, comments))      
    }
    
    val withCompress = SSHOptions("localhost", "test", compress=None)
    val noCompress = SSHOptions("localhost", "test", compress=Some(9))
    
    SSH.once(withCompress) (toTest(withReusedSFTP, 1, 100*1024, "byterates using SFTP (max compression)"))
    SSH.once(noCompress) (toTest(withReusedSFTP, 1, 100*1024, "byterates using SFTP (no compression)"))
  }

  //==========================================================================================================
  test("tunneling test remote->local") {
    SSH.once("localhost", "test", port=22) { ssh1 =>
      ssh1.remote2Local(22022, "localhost", 22)
      SSH.once("localhost", "test", port=22022) { ssh2 => 
        ssh2.executeAndTrim("echo 'works'") should equal("works")
      }
    }
  }
  
  //==========================================================================================================
  test("tunneling test local->remote") {
    SSH.once("localhost", "test", port=22) { ssh1 =>
      ssh1.local2Remote(33033, "localhost", 22) // TODO - hmmmm param mismatch... check that
      SSH.once("localhost", "test", port=33033) { ssh2 => 
        ssh2.executeAndTrim("echo 'works'") should equal("works")
      }
    }
  }
  
}

