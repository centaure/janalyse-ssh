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
import SSH._
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
    connect(username = "test") { _ execute "expr 1 + 1" } should equal("2\n")
    connect(username = "test") { _ executeAndTrim "expr 1 + 1" } should equal("2")
    connect(username = "test") { _ executeAndTrim "echo 1" :: "echo 2" :: Nil } should equal("1" :: "2" :: Nil)
    val year = connect(username = "test") { _ executeAndTrim "expr 1 + 10" toInt }
    year should equal(11)
  }
  //==========================================================================================================
  test("Execution & file transferts within the same ssh session (autoclose)") {
    connect(username = "test") { ssh =>
      val msg = ssh execute "/bin/echo -n 'Hello %s'".format(util.Properties.userName)

      ssh.put(msg, "HelloWorld.txt")

      (ssh get "HelloWorld.txt") should equal(Some(msg))

      ssh.receive("HelloWorld.txt", "/tmp/sshtest.txt")

      Source.fromFile("/tmp/sshtest.txt").getLines().next() should equal(msg)
    }
  }

  val howmany=100
  //==========================================================================================================
  ignore("Bad performances obtained without persistent schell ssh channel (autoclose)") {
    connect(username = "test") { ssh =>
      val remotedate = ssh execute "date"
      val (dur, _) = howLongFor(() =>
        for (i <- 1 to howmany) { ssh execute "ls -d /tmp && echo 'done'" })
      val throughput = howmany.doubleValue() / dur * 1000
      info("Performance without channel persistency : %.1f cmd/s".format(throughput))
    }
  }
  //==========================================================================================================
  test("Best performance is achieved with mutiple command within the same shell channel (autoclose)") {
    connect(username = "test") {
      _.shell { sh =>
        val remotedate = sh execute "date"
        val (dur, _) = howLongFor(() =>
          for (i <- 1 to howmany) { sh execute "ls -d /tmp && echo 'done'" })
        val throughput = howmany.doubleValue() / dur * 1000
        info("Performance with channel persistency : %.1f cmd/s".format(throughput))
      }
    }
  }
  //==========================================================================================================
  test("Start a remote process in background") {
    import fr.janalyse.ssh.SSH
    SSH.connect(username = "test") { implicit ssh =>

      val tester = self
      val receiver = actor {
        var lines = List.empty[String]
        loop {
          react {
            case e: StandardErrorMessage => info("STDERR" + e.line)
            case e: StandardOutputMessage => lines = e.line :: lines
            case _: StandardOutputClosed => tester ! lines.reverse; exit
            case _: StandardErrorClosed =>
          }
        }
      }

      val executor = ssh.run("vmstat 1 5", receiver)

      receive {
        case x: List[String] =>
          x.zipWithIndex map { case (l, i) => info("%d : %s".format(i, l)) }
          x.size should equal(7)
      }
      
      executor.close()
    }
  }

  //==========================================================================================================
  test("Usage case example - for tutorial") {
    import fr.janalyse.ssh.SSH
    SSH.connect(host = "localhost", username = "test") { ssh =>
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
      val caller = self
      val receiver = actor {
        loop {
          react {
            case e: StandardOutputMessage => println(e.line)
            case _: StandardOutputClosed => caller ! "That's all folks"; exit
            case _ =>
          }
        }
      }
      val executor = ssh.run("vmstat 1 3", receiver)
      receive {case _ => }
      executor.close()
    }
  }

  //==========================================================================================================
  ignore("SSHAPI process must exit naturally, when no operation is in progress") {
    import collection.JavaConversions._
    import java.io.File.{separator=>FS, pathSeparator=>PS}
    import scala.sys.process._
    val rt = Runtime.getRuntime()
    
    val classpath  = Properties.javaClassPath+PS+"/opt/scala/"+FS+"lib"+FS+"scala-library.jar"
    
    val env        = System.getenv()
    val cwd        = new File(".")
    val javacmd    = Properties.javaHome+FS+"bin"+FS+"java"
    val cmd        = javacmd::"-classpath"::classpath::"fr.janalyse.ssh.SubProcessTest"::Nil
    val subprocbd  = Process(cmd, Some(cwd), env.toList:_*)
    var stdout     = StringBuilder.newBuilder
    var stderr     = StringBuilder.newBuilder
    val proclogger = ProcessLogger(stdout.append(_), stderr.append(_))
    val proc       = subprocbd.run(proclogger)
    Thread.sleep(2000)

    val err=stderr.toString()
    
    if (err.size>0) info(err)
    
    err should have length(0)
    
    subprocbd.hasExitValue should equal(true)
    
    stdout.toString.contains("20") should equal(true)
    
    proc.exitValue should equal (0)
    
  }
  //==========================================================================================================

  
}






/*
 *  THE FOLLOWING PROBLEM HAS BEEN SOLVED BY REMOVING messages sending from external threads... 
 *  from out of actor context.
 * 
Process hangs on :
 
Name: main
State: WAITING on java.lang.Thread@404eb2
Total blocked: 4  Total waited: 6

Stack trace: 
java.lang.Object.wait(Native Method)
java.lang.Thread.join(Thread.java:1186)
java.lang.Thread.join(Thread.java:1239)
scala.tools.nsc.util.package$$anonfun$waitingForThreads$1.apply(package.scala:30)
scala.tools.nsc.util.package$$anonfun$waitingForThreads$1.apply(package.scala:30)
scala.collection.immutable.HashSet$HashSet1.foreach(HashSet.scala:130)
scala.collection.immutable.HashSet$HashTrieSet.foreach(HashSet.scala:275)
scala.tools.nsc.util.package$.waitingForThreads(package.scala:30)
scala.tools.nsc.ScriptRunner.withCompiledScript(ScriptRunner.scala:130)
scala.tools.nsc.ScriptRunner.runScript(ScriptRunner.scala:188)
scala.tools.nsc.ScriptRunner.runScriptAndCatch(ScriptRunner.scala:201)
scala.tools.nsc.MainGenericRunner.runTarget$1(MainGenericRunner.scala:58)
scala.tools.nsc.MainGenericRunner.process(MainGenericRunner.scala:80)
scala.tools.nsc.MainGenericRunner$.main(MainGenericRunner.scala:89)
scala.tools.nsc.MainGenericRunner.main(MainGenericRunner.scala)
// ----------------------------
 -Yrepl-sync : 
 */


object SubProcessTest {
  def main(args:Array[String]) {
    connect(username = "test") {ssh => 
      println(ssh execute "expr 10 + 10")
    }
  }
  
}

