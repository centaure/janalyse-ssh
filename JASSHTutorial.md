# Introduction #

JASSH Scala SSH API tutorial. All examples described in this document can be directly tested using scala console. Several simplified ways are possible to start the console :
  * using sbt :
    * svn checkout http://janalyse-ssh.googlecode.com/svn/trunk/ jassh
    * cd jassh
    * sbt console
  * download "jassh.jar" file, and start the console as follow :
    * wget http://code.google.com/p/janalyse-ssh/downloads/detail?name=jassh.jar
    * java -jar jassh.jar

Besides that, you can also create your own scala scripts very simply, just create a dummy.scala script file as follow and make it executable (we assume that jassh.jar is in the current directory):
```
$ cat >dummy.scala
#!/bin/sh
exec java -jar jassh.jar "$0" "$@"
!#
jassh.SSH.shell("localhost", "test", "testtest") { sh =>
  print(sh.execute("""echo "Hello World from `hostname`" """))
}
**ctrl-d**
$ chmod a+x dummy.scala
$ ./dummy.scala
Hello World from lanfeust
$ 
```


# First example : Console usage #

Assuming that a user "test" exists with "testtest" as password; if no password is given then jassh will try to use public key authentication.
```
scala> import jassh._
import jassh._

scala> val ssh=SSH("localhost", "test", "testtest")
ssh: fr.janalyse.ssh.SSH = fr.janalyse.ssh.SSH@6798e5f7

scala> ssh.execute("ls")
res0: java.lang.String = 
HelloWorld.txt
file.tst
sshapitest-dummydir
sshapitest.dummy
sshapittest.dummy

scala> val uname  = ssh.execute("uname -a")
uname: java.lang.String = Linux lanfeust 3.2.1-gentoo-r2 #1 SMP Sat Jan 28 12:15:33 CET 2012 x86_64 AMD Phenom(tm) II X6 1090T Processor AuthenticAMD GNU/Linux

scala> ssh.put("Hello World\nToo good", "hello.txt")

scala> for(msg <- ssh.get("hello.txt") ) println(msg)
Hello World
Too good

```

Still using console mode, but now let's use method helpers :

```
$ java -jar jassh.jar

scala> import jassh._
import jassh._

scala> val ssh = SSH("localhost", "test", "testtest")
ssh: fr.janalyse.ssh.SSH = fr.janalyse.ssh.SSH@2cf5006

scala> val sh = ssh.newShell
sh: fr.janalyse.ssh.SSHShell = fr.janalyse.ssh.SSHShell@3775ace6

scala> import sh._
import sh._

scala> date
res0: java.util.Date = Wed Jul 04 21:40:31 CEST 2012

scala> pwd
res0: String = /home/test

scala> cd("sshapitest-dummydir")

scala> pwd
res3: String = /home/test/sshapitest-dummydir

scala> ls
res4: Iterable[String] = WrappedArray("")

scala> cd()

scala> pwd
res6: String = /home/test

scala> ls
res7: Iterable[String] = WrappedArray(HelloWorld.txt, authorized_keys, file.tst, heapdump-1906-Elive.hprof, hello.txt, sshapitest-dummydir, sshapitest.dummy, sshapittest.dummy, test-transfert, testme.txt)

scala> uname
res8: String = Linux

scala> hostname
res9: String = lanfeust

scala> md5sum("HelloWorld.txt")
res10: Option[String] = Some(ec245c490ec135c115d190165956c92b)

scala> sha1sum("heapdump-1906-Elive.hprof")
res11: Option[String] = Some(21f9fbab210a7314992114daf7421af90df8aaab)

scala> fileSize("heapdump-1906-Elive.hprof")
res12: Option[Long] = Some(597369928)

```

# Second example : Script usage #

```
```

# Configuration #

# Remote execution #

# Remote background execution #

# Remote transfert #

# Helper methods #
I've made available a large set of methods to simplify common usages, all available directly on both SSH and SSHShell classes :
  * fileSize : get remote file size in bytes
  * md5sum : returns remote file md5 sum
  * sha1sum : returns remote file sha1 sum
  * uname : get remote system uname
  * osname : get remote system uname in lowercase
  * ls : returns an iterable of filenames
  * cd : Modify the remote current working directory (For SSHShell, not meaningfull for SSH)
  * pwd : Get the remote current working directory
  * hostname : remote hostname
  * date : remote current date
  * findAfterDate : returns an Iterable of filenames modified after the a specified date
  * test : execute a test shell expression, returns true or false
  * exists : does the remote filename exists ?
  * isDirectory : is the remote file a directory ?
  * isFile : is the remote file a regular file ?
  * isExecutable : is the remote file executable ?
  * du : how many kilobytes used by a given file tree
  * ps : get process list
  * fsFreeSpace : how many space remain free in the filesystem of the give file
  * fileRights : return file rights for the given filepath
  * env : get environment variables as a Map
  * osid : get OS id (either AIX, Linux, Darwin, SunOS object)
  * rm : delete files
  * rmdir : delete a directory
  * mkdir : create a directory
  * arch : print OS architecture
  * kill : kill processes