# JASSH : High level scala SSH API for easy and fast operations on remote servers. #

[\*\*\*\* PROJECT MOVED TO GITHUB \*\*\*\*](https://github.com/dacr/jassh)

This API is [JSCH](http://www.jcraft.com/jsch/) based. Interfaces are stable. Many helper functions are provided to simplify unix operations (<a href='http://www.janalyse.fr/scaladocs/janalyse-ssh/#fr.janalyse.ssh.ShellOperations'>ps, ls, cat, kill, find, ...</a>), an other goal of this API is to create an unix abstraction layer (Linux, Aix, Solaris, Darwin, ...).

[A tutorial is available here](http://code.google.com/p/janalyse-ssh/wiki/JASSHTutorial)

A standalone packaging, "[jassh.jar](http://code.google.com/p/janalyse-ssh/downloads/detail?name=jassh.jar)", is provided, for quick console or scripting usage. It embeds all dependencies, and comes with all scala libraries. Otherwise for library usage, just add the dependency and the repository to your sbt configuration file (build.sbt). Just run "java -jar jassh.jar" to start the console, see a usage example below.

**JAnalyse software maven repository** : http://www.janalyse.fr/repository/

**Scala docs** : http://www.janalyse.fr/scaladocs/janalyse-ssh

**Current releases** :  **0.9.10** (for scala 2.10)   **0.9.5b3** (for scala 2.9.1, 2.9.2)

**Releases notes** : http://code.google.com/p/janalyse-ssh/wiki/ReleaseNotes

**Declare dependency in SBT as follow** :
`libraryDependencies += "fr.janalyse"   %% "janalyse-ssh" % "0.9.10" % "compile"`

**Add JAnalyse repository in SBT as follow** :
`resolvers += "JAnalyse Repository" at "http://www.janalyse.fr/repository/"`


---


**hello world script** : It requires a local user named "test" with password "testtest", remember that you can remove the password, if your public key has been added in authorized\_keys file of the test user.
```
#!/bin/sh
exec java -jar jassh.jar "$0" "$@"
!#
jassh.SSH.shell("localhost", "test", "testtest") { sh =>
  print(sh.execute("""echo "Hello World from `hostname`" """))
}

```

**console usage example** :
```
$ java -jar jassh.jar
scala> import jassh._
import jassh._

scala> val found = SSH.once("localhost", "test") {_ executeAndTrimSplit "find /usr/share -name '*.jpg'"}
found: Array[java.lang.String] = Array(/usr/share/gtkhtml-3.14/icons/midnight-stars.jpg, ...

scala> 
```


**remote vmstat script** :
_(The following script assume that the current user has already automatic access to the given remote host using SSH public key authentication.)_
```
#!/bin/sh
exec java -jar jassh.jar "$0" "$@"
!#
val host=if (args.size>0) args(0) else "localhost"
val user=if (args.size>1) args(1) else util.Properties.userName
val freq=if (args.size>2) args(2) else "2"
val numb=if (args.size>3) args(3) else "10"
val vmstatcmd="vmstat %s %s".format(freq, numb)

import fr.janalyse.ssh._

SSH.once(host, user)  {
  _.run(vmstatcmd, {case ExecPart(content) => println(content) case _ => }).waitForEnd
}

```

**future usage example** :
```
#!/bin/sh
exec java -jar jassh.jar "$0" "$@"
!#
// jassh.jar can be downloaded here : http://code.google.com/p/janalyse-ssh/

import jassh._
import concurrent._
import duration._
import scala.util._

val remotehosts = (1 to 10) map { num =>
  SSHOptions("127.0.0.1", username="test", password="testtest", name=Some(s"host#$num"))
}

// Custom executor, with this executor no need for a final Await.ready
import java.util.concurrent.{ Executors, ThreadPoolExecutor, TimeUnit }
implicit val customEC = ExecutionContext.fromExecutorService(
  Executors.newCachedThreadPool() match {
    case e: ThreadPoolExecutor =>
      //Too allow a quick exit from this script because default value is 60s
      e.setKeepAliveTime(2, TimeUnit.SECONDS)
      e
    case x => x
  }
)

val futures = remotehosts.map { rh =>
  future {
    SSH.once(rh) { ssh =>
      ssh.execute(s"""sleep 1 ; echo 'Hello from ${rh.name getOrElse "default"}'""")
    }
  }
}

val onefuture = Future.sequence(futures)

onefuture onComplete {
   case Failure(err) => println(err.getMessage)
   case Success(messages) => println(messages.mkString("\n"))
}

```

**Usages example** :

```
  test("Usage case example - for tutorial") {
    import fr.janalyse.ssh.SSH
    SSH.once(sshopts) { ssh =>
      
      val uname = ssh executeAndTrim "uname -a"
      val fsstatus = ssh execute "df -m"
      val fmax = ssh get "/etc/lsb-release" // Warning SCP only work with regular file
      
      ssh.shell { sh => // For higher performances
        val hostname = sh.executeAndTrim("hostname")
        val files = sh.execute("find /usr/lib/")
      }
      ssh.ftp { ftp => // For higher performances
        val cpuinfo = ftp.get("/proc/cpuinfo")
        val meminfo = ftp.get("/proc/meminfo")
      }
      // output streaming
      def receiver(result:ExecResult) {result match {case ExecPart(m) => println(m) case _ =>}}
      val executor = ssh.run("for i in 1 2 3 ; do echo hello$i ; done", receiver)
      executor.waitForEnd
    }
  }
  
  //==========================================================================================================
  test("Simultaenous SSH operations") {    
    val started = System.currentTimeMillis()
    val cnxinfos = List(sshopts, sshopts, sshopts, sshopts, sshopts)
    val sshs   = cnxinfos.par map {SSH(_)}
    
    sshs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(6))
    
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
      uname.toLowerCase       should (equal("linux") or equal("darwin") or equal("aix") or equal("sunos"))
      osname                  should (equal("linux") or equal("darwin") or equal("aix") or equal("sunos"))
      osid                    should (equal(Linux) or equal(Darwin) or equal(AIX) or equal(SunOS))
      env.size                should be > (0)
      hostname                should equal(rhostname)
      fileSize(testfile)      should equal(Some(4))
      md5sum(testfile)        should equal(Some("f71dbe52628a3f83a77ab494817525c6"))
      md5sum(testfile)        should equal(Some(SSHTools.md5sum("toto")))
      sha1sum(testfile)       should equal(Some("0b9c2625dc21ef05f6ad4ddf47c5f203837aa32c"))
      ls                      should contain(testfile)
      cd(testdir)
      pwd                     should equal(homedir+"/"+testdir)
      cd
      pwd                     should equal(homedir)
      sh.test("1 == 1")       should equal(true)
      sh.test("1 == 2")       should equal(false)
      isFile(testfile)        should equal(true)
      isDirectory(testfile)   should equal(false)
      exists(testfile)        should equal(true)
      isExecutable(testfile)  should equal(false)
      findAfterDate(".", started) should (have size(1) or have size(2)) // Added 2 because of .bash_history
      val reftime = now.getTime
      date().getTime          should (be>(reftime-1000) and be<(reftime+1000))
      fsFreeSpace("/tmp")     should be('defined)
      fileRights("/tmp")      should be('defined)
      ps().filter(_.cmdline contains "java").size should be >(0)
      du("/bin").value        should be >(0L)
    }
  }

```


Tests results on a AMD Phenom(tm) II X6 1090T, 3200Mhz, up to 567 commands per seconds and 46Mb/s !

```
[info] ExternalSSHAPITest:
[info] SSHAPITest:
[info] - Hello 1
[info] - Hello 2
[info] - Hello 3
[info] - One line exec with automatic resource close
[info] - Hello 4
[info] - Hello 5
[info] - Execution & file transferts within the same ssh session (autoclose)
[info] - shell coherency check
[info] - shell coherency check with long command lines (in //)
[info] - SSHShell : Bad performances obtained without persistent schell ssh channel (autoclose)
[info]   + Performance using shell without channel persistency : 38,0 cmd/s  
[info]   + Performance using shell without channel persistency : 38,2 cmd/s with VTY 
[info] - SSHShell : Best performance is achieved with mutiple command within the same shell channel (autoclose)
[info]   + Performance using with channel persistency : 558,5 cmd/s  
[info]   + Performance using with channel persistency : 566,6 cmd/s with VTY 
[info] - SSHExec : performances obtained using exec ssh channel (no persistency)
[info]   + Performance using exec ssh channel (no persistency) : 91,0 cmd/s  
[info]   + Performance using exec ssh channel (no persistency) : 47,6 cmd/s with VTY 
[info] - Start a remote process in background
[info]   + 0 : hello 
[info]   + 1 : hello 
[info]   + 2 : hello 
[info]   + 3 : hello 
[info]   + 4 : hello 
[info] - Usage case example - for tutorial
[info] - Simultaenous SSH operations
[info]   + Fri Feb 22 21:32:08 CET 2013----Fri Feb 22 21:32:08 CET 2013----Fri Feb 22 21:32:08 CET 2013----Fri Feb 22 21:32:08 CET 2013----Fri Feb 22 21:32:08 CET 2013 
[info] - Simplified persistent ssh shell usage
[info] - Simplified persistent ssh shell and ftp usage
[info] - simplified usage with sshOptions as Option !!! IGNORED !!!
[info] - timeout tests !!! IGNORED !!!
[info] - helper methods
[info] - file transfert performances (with content loaded in memory) !!! IGNORED !!!
[info] - ssh compression
[info]   + Bytes rate : 43,9Mb/s 100Mb in 2,3s for 1 files - byterates using SFTP (max compression) 
[info]   + Bytes rate : 45,4Mb/s 100Mb in 2,2s for 1 files - byterates using SFTP (no compression) 
[info] - tunneling test remote->local
[info] - tunneling test local->remote
[info] - tunneling test intricated tunnels
[info] - remote ssh sessions (ssh tunneling ssh
[info] - SCP/SFTP and special system file !!! IGNORED !!!
[info] - sharing SSH options...
[info] - env command test
[info] Passed: : Total 29, Failed 0, Errors 0, Passed 25, Skipped 4

```