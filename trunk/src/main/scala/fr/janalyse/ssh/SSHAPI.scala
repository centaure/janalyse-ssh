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

import com.jcraft.jsch._
import scala.io.BufferedSource
import java.util.logging._
import java.io._
import scala.collection.generic.CanBuildFrom
import java.nio.charset.Charset
import java.nio.ByteBuffer
import scala.util.{ Properties => SP }
import java.io.File.{ separator => FS, pathSeparator => PS }
import scala.collection.mutable.SynchronizedQueue
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.locks.LockSupport
import com.typesafe.scalalogging.slf4j.Logging

import language.implicitConversions
import language.reflectiveCalls


// ==========================================================================================

/**
 * SSHCommand class models ssh command
 * @author David Crosson
 */
class SSHCommand(val cmd: String) {
}

/**
 * SSHCommand object implicit conversions container
 * @author David Crosson
 */
object SSHCommand {
  implicit def stringToCommand(cmd: String) = new SSHCommand(cmd)
}

// ==========================================================================================

/**
 * SSHBatch class models ssh batch (in fact a list of commands)
 * @author David Crosson
 */
class SSHBatch(val cmdList: Iterable[String]) {
}

/**
 * SSHBatch object implicit conversions container
 * @author David Crosson
 */
object SSHBatch {
  implicit def stringListToBatchList(cmdList: Iterable[String]) = new SSHBatch(cmdList)
}

// ==========================================================================================

/**
 * SSHRemoteFile class models a file on the remote system
 * @author David Crosson
 */
class SSHRemoteFile(val remoteFilename: String) {
  def get(implicit ssh: SSH) = {
    ssh.ftp { _ get remoteFilename }
  }
  def put(data: String)(implicit ssh: SSH) = {
    ssh.ftp { _ put (data, remoteFilename) }
  }
  def >>(toLocalFilename: String)(implicit ssh: SSH) = {
    ssh.ftp { _.receive(remoteFilename, toLocalFilename) }
  }
  def <<(fromLocalFilename: String)(implicit ssh: SSH) = {
    ssh.ftp { _.send(fromLocalFilename, remoteFilename) }
  }
}

/**
 * SSHRemoteFile object implicit conversions container
 * @author David Crosson
 */
object SSHRemoteFile {
  implicit def stringToRemoteFile(filename: String) = new SSHRemoteFile(filename)
}





// ==========================================================================================

/**
 * SSHPassword class models a password, that may be given or not
 * @author David Crosson
 */
case class SSHPassword(password: Option[String]) {
  override def toString = password getOrElse ""
}

/**
 * NoPassword object to be used when no password is given
 * @author David Crosson
 */
object NoPassword extends SSHPassword(None)

/**
 * SSHPassword object implicit conversions container
 * @author David Crosson
 */
object SSHPassword {
  implicit def string2password(pass: String) = pass match {
    case "" => NoPassword
    case password => SSHPassword(Some(pass))
  }
  implicit def stringOpt2password(passopt: Option[String]) = passopt match {
    case Some(password) => string2password(password)
    case None => NoPassword
  }
}


// ==========================================================================================

/**
 * SSH object factories
 * @author David Crosson
 */
object SSH {

  /**
   * Executes the given code then closes the new ssh associated session.
   * @param host ip address or hostname
   * @param username user name
   * @param password user password (if ommitted, will try public key authentication)
   * @param passphrase keys passphrase (if required)
   * @param port remote ssh port
   * @param timeout timeout
   * @param withssh code bloc to execute
   * @return "withssh" returns type
   */
  def once[T](
    host: String = "localhost",
    username: String = util.Properties.userName,
    password: SSHPassword = NoPassword,
    passphrase: SSHPassword = NoPassword,
    port: Int = 22,
    timeout: Int = 300000)(withssh: (SSH) => T): T = using(new SSH(SSHOptions(username = username, password = password, passphrase = passphrase, port = port, timeout = timeout)(host = host))) {
    withssh(_)
  }
  /**
   * Executes the given code then closes the new ssh associated session.
   * @param options ssh options
   * @param withssh code bloc to execute
   * @return "withssh" returns type
   */
  def once[T](options: SSHOptions)(withssh: (SSH) => T): T = using(new SSH(options)) {
    withssh(_)
  }
  /**
   * Executes the given code then closes the new ssh associated session.
   * @param someOptions Some ssh options or None, if None is given, nothing will be done
   * @param withssh code bloc to execute
   * @return "withssh" returns type
   */
  def once[T](someOptions: Option[SSHOptions])(withssh: (SSH) => T): Option[T] = someOptions map { options =>
    using(new SSH(options)) {
      withssh(_)
    }
  }

  /**
   * Executes the given code then closes the new ssh shell channel associated session.
   * @param host ip address or hostname
   * @param username user name
   * @param password user password (if ommitted, will try public key authentication)
   * @param passphrase keys passphrase (if required)
   * @param port remote ssh port
   * @param timeout timeout
   * @param withsh code bloc to execute
   * @return "withsh" returns type
   */
  def shell[T](
    host: String = "localhost",
    username: String = util.Properties.userName,
    password: SSHPassword = NoPassword,
    passphrase: SSHPassword = NoPassword,
    port: Int = 22,
    timeout: Int = 300000)(withsh: (SSHShell) => T): T = shell[T](SSHOptions(username = username, password = password, passphrase = passphrase, port = port, timeout = timeout)(host = host))(withsh)

  /**
   * Executes the given code then closes the new ssh shell associated session.
   * @param options ssh options
   * @param withsh code bloc to execute
   * @return "withssh" returns type
   */
  def shell[T](options: SSHOptions)(withsh: (SSHShell) => T): T = using(new SSH(options)) { ssh =>
    ssh.shell { sh => withsh(sh) }
  }
  /**
   * Executes the given code then closes the new ssh shell associated session.
   * @param someOptions Some ssh options or None, if None is given, nothing will be done
   * @param withsh code bloc to execute
   * @return "withssh" returns type
   */
  def shell[T](someOptions: Option[SSHOptions])(withsh: (SSHShell) => T): Option[T] = someOptions map { shell[T](_)(withsh) }

  /**
   * Executes the given code then closes the new ssh ftp channel associated session.
   * @param host ip address or hostname
   * @param username user name
   * @param password user password (if ommitted, will try public key authentication)
   * @param passphrase keys passphrase (if required)
   * @param port remote ssh port
   * @param timeout timeout
   * @param withftp code bloc to execute
   * @return "withftp" returns type
   */
  def ftp[T](
    host: String = "localhost",
    username: String = util.Properties.userName,
    password: SSHPassword = NoPassword,
    passphrase: SSHPassword = NoPassword,
    port: Int = 22,
    timeout: Int = 300000)(withftp: (SSHFtp) => T): T = ftp[T](SSHOptions(username = username, password = password, passphrase = passphrase, port = port, timeout = timeout)(host = host))(withftp)

  /**
   * Executes the given code then closes the new sftp associated session.
   * @param options ssh options
   * @param withftp code bloc to execute
   * @return "withftp" returns type
   */
  def ftp[T](options: SSHOptions)(withftp: (SSHFtp) => T): T = using(new SSH(options)) { ssh =>
    ssh.ftp { ftp => withftp(ftp) }
  }

  /**
   * Executes the given code then closes the new sftp associated session.
   * @param someOptions Some ssh options or None, if None is given, nothing will be done
   * @param withftp code bloc to execute
   * @return "withftp" returns type
   */
  def ftp[T](someOptions: Option[SSHOptions])(withftp: (SSHFtp) => T): Option[T] = someOptions map { ftp[T](_)(withftp) }

  /**
   * Executes the given code then closes the new ssh shell and ftp channels associated sessions.
   * @param host ip address or hostname
   * @param username user name
   * @param password user password (if ommitted, will try public key authentication)
   * @param passphrase keys passphrase (if required)
   * @param port remote ssh port
   * @param timeout timeout
   * @param withshftp code bloc to execute
   * @return "withshftp" returns type
   */
  def shellAndFtp[T](
    host: String = "localhost",
    username: String = util.Properties.userName,
    password: SSHPassword = NoPassword,
    passphrase: SSHPassword = NoPassword,
    port: Int = 22,
    timeout: Int = 300000)(withshftp: (SSHShell, SSHFtp) => T): T = shellAndFtp[T](SSHOptions(username = username, password = password, passphrase = passphrase, port = port, timeout = timeout)(host = host))(withshftp)

  /**
   * Executes the given code then closes the new ssh shell and sftp associated sessions.
   * @param options ssh options
   * @param withshftp code bloc to execute
   * @return "withshftp" returns type
   */
  def shellAndFtp[T](options: SSHOptions)(withshftp: (SSHShell, SSHFtp) => T): T = using(new SSH(options)) { ssh =>
    ssh.shell { sh => ssh.ftp { ftp => withshftp(sh, ftp) } }
  }
  /**
   * Executes the given code then closes the new ssh shell and sftp associated sessions.
   * @param someOptions Some ssh options or None, if None is given, nothing will be done
   * @param withshftp code bloc to execute
   * @return "withshftp" returns type
   */
  def shellAndFtp[T](someOptions: Option[SSHOptions])(withshftp: (SSHShell, SSHFtp) => T): Option[T] = someOptions map { shellAndFtp[T](_)(withshftp) }

  /**
   * Creates a new SSH session, it is up to the user to manage close
   * @param host ip address or hostname
   * @param username user name
   * @param password user password (if ommitted, will try public key authentication)
   * @param passphrase keys passphrase (if required)
   * @param port remote ssh port
   * @param timeout timeout
   * @return SSH session
   */
  def apply(
    host: String = "localhost",
    username: String = util.Properties.userName,
    password: SSHPassword = NoPassword,
    passphrase: SSHPassword = NoPassword,
    port: Int = 22,
    timeout: Int = 300000) = new SSH(SSHOptions(username = username, password = password, passphrase = passphrase, port = port, timeout = timeout)(host = host))

  /**
   * Creates a new SSH session, it is up to the user to manage close
   * @param options ssh options
   * @return SSH session
   */
  def apply(options: SSHOptions) = new SSH(options)

  /**
   * Creates a new SSH session, it is up to the user to manage close
   * @param someOptions Some ssh options or None, if None is given, nothing will be done
   * @return Some SSH session or None
   */
  def apply(someOptions: Option[SSHOptions]): Option[SSH] = someOptions map { new SSH(_) }

  protected def using[T <: { def close() }, R](resource: T)(block: T => R) = {
    try block(resource)
    finally resource.close
  }

}

/**
 * SSH class. This class is the main entry point to the API
 * @author David Crosson
 */
class SSH(val options: SSHOptions) extends ShellOperations with TransfertOperations {
  private implicit val ssh = this
  private val jsch = new JSch
  val jschsession: Session = {
    options.keyfiles2lookup
      .toStream
      .map(new File(options.sshUserDir, _))
      .filter(_.exists)
      .foreach(f => jsch.addIdentity(f.getAbsolutePath))

    val ses = jsch.getSession(options.username, options.host, options.port)
    ses.setServerAliveInterval(2000)
    ses.setTimeout(options.connectTimeout.toInt) // Timeout for the ssh connection (unplug cable to simulate) 
    ses.setUserInfo(SSHUserInfo(options.password.password, options.passphrase.password))
    ses.connect(options.connectTimeout.toInt)
    if (ssh.options.noneCipher) {
      /* Default : jsch 0.1.48 (2012-06-26)
		cipher.s2c=aes128-ctr,aes128-cbc,3des-ctr,3des-cbc,blowfish-cbc,aes192-cbc,aes256-cbc
		cipher.c2s=aes128-ctr,aes128-cbc,3des-ctr,3des-cbc,blowfish-cbc,aes192-cbc,aes256-cbc
       */
      ses.setConfig("cipher.s2c", "none,aes128-cbc,3des-cbc,blowfish-cbc")
      ses.setConfig("cipher.c2s", "none,aes128-cbc,3des-cbc,blowfish-cbc")
      ses.rekey()
    }
    if (ssh.options.compress.isDefined) {
      ses.setConfig("compression.s2c", "zlib@openssh.com,zlib,none");
      ses.setConfig("compression.c2s", "zlib@openssh.com,zlib,none");
      ses.setConfig("compression_level", ssh.options.compress.get.toString);
    } else {
      ses.setConfig("compression.s2c", "none,zlib@openssh.com,zlib");
      ses.setConfig("compression.c2s", "none,zlib@openssh.com,zlib");
      ses.setConfig("compression_level", "0");
    }
    ses
  }

  def shell[T](proc: (SSHShell) => T) = SSH.using(new SSHShell) { proc(_) }

  def ftp[T](proc: (SSHFtp) => T) = SSH.using(new SSHFtp) { proc(_) }

  def scp[T](proc: (SSHScp) => T) = SSH.using(new SSHScp) { proc(_) }

  def noerr(data: ExecResult) {}

  def run(cmd: String, out: ExecResult => Any, err: ExecResult => Any = noerr) = new SSHExec(cmd, out, err)

  override def execute(cmd: SSHCommand) =
    //shell { _ execute cmd.cmd }    // Using SSHShell channel  (lower performances)
    execOnce(cmd) // Using SSHExec channel (better performances)

  override def executeAll(cmds: SSHBatch) = shell { _ executeAll cmds }

  def execOnceAndTrim(scmd: SSHCommand) = execOnce(scmd).trim()

  def execOnce(scmd: SSHCommand) = {
    val stdout = new StringBuilder()
    val stderr = new StringBuilder()
    def outputReceiver(buffer:StringBuilder)(content: ExecResult) {
      content match {
        case ExecPart(part) =>
          if (buffer.size > 0) buffer.append("\n")
          buffer.append(part)
        case ExecEnd =>
        case ExecTimeout =>
      }
    }
    var runner: Option[SSHExec] = None
    try {
      runner = Some(new SSHExec(scmd.cmd, outputReceiver(stdout), outputReceiver(stderr)))
      runner foreach { _.waitForEnd }
    } catch {
      case e:InterruptedException =>
        throw new SSHTimeoutException(stdout.toString, stderr.toString)
    } finally {
      runner foreach { _.close }
    }
    stdout.toString()
  }

  private var firstHasFailed=false
  
  private def opWithFallback[T]( primary : => T, fallback: => T ):T = {
    if (firstHasFailed) fallback
    else {
      try {
        primary
      } catch {
        case x:RuntimeException if x.getMessage contains "SSH transfert protocol error" =>
          firstHasFailed=true
          fallback
      }
    }
  }
  
  override def get(remoteFilename: String): Option[String] =
    opWithFallback( 
        ssh.scp(_ get remoteFilename),
        ssh.ftp(_ get remoteFilename)
    )


  override def getBytes(remoteFilename: String): Option[Array[Byte]] =
    opWithFallback( 
      ssh.scp( _ getBytes remoteFilename),
      ssh.ftp( _ getBytes remoteFilename)
    )

  override def receive(remoteFilename: String, toLocalFile: File) {
    opWithFallback(
      ssh.scp(_.receive(remoteFilename, toLocalFile)),
      ssh.ftp(_.receive(remoteFilename, toLocalFile))
    )
  }

  override def put(data: String, remoteDestination: String) {
    opWithFallback( 
      ssh.scp(_ put (data, remoteDestination)),
      ssh.ftp(_ put (data, remoteDestination))
    )
  }

  override def putBytes(data: Array[Byte], remoteDestination: String) {
    opWithFallback( 
      ssh.scp(_ putBytes (data, remoteDestination)),
      ssh.ftp(_ putBytes (data, remoteDestination))
    )
  }

  override def send(fromLocalFile: File, remoteDestination: String) {
    opWithFallback(
      ssh.scp(_.send(fromLocalFile, remoteDestination)),
      ssh.ftp(_.send(fromLocalFile, remoteDestination))
    )
  }

  /**
   * Remote host/port => local port (client-side)
   * @param lport remote host port will be mapped on this port on client side (bound to localhost)
   * @param host  remote host (accessed through ssh server side)
   * @param hport remote port (on remote host) to bring back locally
   */
  def remote2Local(lport: Int, host: String, hport: Int) = {
    jschsession.setPortForwardingL(lport, host, hport)
  }

  /**
   * Remote host/port => local port (client-side automatically chosen)
   * @param lport remote host port will be mapped on this port on client side (bound to localhost)
   * @param host  remote host (accessed through ssh server side)
   * @param hport remote port (on remote host) to bring back locally
   * @return chosen local listening port
   */
  def remote2Local(host: String, hport: Int) = {
    jschsession.setPortForwardingL(0, host, hport)
  }

  /**
   * Local (client-side) host/port => Remote host with specified port
   * @param rport the port to create on remote server (where the ssh server stands) to forward lhost/lport
   * @param lhost local host (accessible from ssh client host) from which we'll forward a port
   * @param lport the port to foward
   */
  def local2Remote(rport: Int, lhost: String, lport: Int) {
    jschsession.setPortForwardingR(rport, lhost, lport);
  }

  /**
   * Get access to a remote SSH through current SSH session
   * @param options ssh options
   * @return SSH session
   */
  def remote(remoteOptions: SSHOptions): SSH = {
    val chosenPort: Int = remote2Local(remoteOptions.host, remoteOptions.port)
    val localOptions = remoteOptions.copy(port = chosenPort)(host = "127.0.0.1")
    new SSH(localOptions)
  }

  /**
   * Get access to a remote SSH through current SSH session
   * @param host ip address or hostname
   * @param username user name
   * @param password user password (if ommitted, will try public key authentication)
   * @param passphrase keys passphrase (if required)
   * @param port remote ssh port
   * @param timeout timeout
   * @return SSH session
   */
  def remote(
    host: String = "localhost",
    username: String = util.Properties.userName,
    password: SSHPassword = NoPassword,
    passphrase: SSHPassword = NoPassword,
    port: Int = 22,
    timeout: Int = 300000): SSH = remote(SSHOptions(username = username, password = password, passphrase = passphrase, port = port, timeout = timeout)(host = host))

  /**
   * returns a new shell for current SSH session, you must manage close operation by your self
   */
  def newShell = new SSHShell

  /**
   * returns a new ftp for current SSH session, you must manage close operation by your self
   * @return sftp instance
   */
  def newSftp = new SSHFtp

  /**
   * close current ssh session
   */
  def close() { jschsession.disconnect }

}

// ==========================================================================================
sealed trait ExecResult
case class ExecPart(content:String) extends ExecResult
object ExecEnd extends ExecResult
object ExecTimeout extends ExecResult


class SSHExec(cmd: String, out: ExecResult => Any, err: ExecResult => Any)(implicit ssh: SSH) {

  private val (channel, stdout, stderr, stdin) = {
    val ch = ssh.jschsession.openChannel("exec").asInstanceOf[ChannelExec]
    ch.setCommand(cmd.getBytes())
    val stdout = ch.getInputStream()
    val stderr = ch.getErrStream()
    val stdin = ch.getOutputStream()
    ch.setPty(ssh.options.execWithPty)
    ch.connect(ssh.options.connectTimeout.toInt)
    (ch, stdout, stderr, stdin)
  }
  private val stdoutThread = InputStreamThread(channel, stdout, out)
  private val stderrThread = InputStreamThread(channel, stderr, err)
  private val timeoutThread = TimeoutManagerThread(ssh.options.timeout) {
    stdoutThread.interrupt()
    stderrThread.interrupt()
    }

  def giveInputLine(line: String) {
    stdin.write(line.getBytes())
    stdin.write("\n".getBytes())
    stdin.flush()
  }

  def waitForEnd {
    stdoutThread.join()
    stderrThread.join()
    if (timeoutThread.interrupted) throw new InterruptedException("Timeout Reached")
    close()
  }

  def close() {
    stdin.close()
    stdoutThread.interrupt()
    stderrThread.interrupt()
    channel.disconnect
    timeoutThread.interrupt()
  }

  private class TimeoutManagerThread(timeout:Long)(todo : =>Any) extends Thread {
    var interrupted=false
    override def run() {
      if (timeout>0) {
	      try {
	        Thread.sleep(timeout)
	        interrupted=true
	        todo
	      } catch {
	        case e:InterruptedException => 
	      }
      }
    }
  }
  private object TimeoutManagerThread {
    def apply(timeout:Long)(todo : => Any):TimeoutManagerThread = {
      val thread = new TimeoutManagerThread(timeout)(todo)
      thread.start()
      thread
    }
  }
  
  private class InputStreamThread(channel: ChannelExec, input: InputStream, output: ExecResult => Any) extends Thread {
    override def run() {
      val bufsize = 16 * 1024
      val charset = Charset.forName(ssh.options.charset)
      val binput = new BufferedInputStream(input)
      val bytes = Array.ofDim[Byte](bufsize)
      val buffer = ByteBuffer.allocate(bufsize)
      val appender = new StringBuilder()
      var eofreached = false
      try {
	      do {
	        // Notes : It is important to try to read something even available == 0 in order to be able to get EOF message !
	        // Notes : After some tests, looks like jsch input stream is probably line oriented... so no need to use available !
	        val howmany = binput.read(bytes, 0, bufsize /*if (available < bufsize) available else bufsize*/ )
	        if (howmany == -1) eofreached = true
	        if (howmany > 0) {
	          buffer.put(bytes, 0, howmany)
	          buffer.flip()
	          val cbOut = charset.decode(buffer)
	          buffer.compact()
	          appender.append(cbOut.toString())
	          var s = 0
	          var e = 0
	          do {
	            e = appender.indexOf("\n", s)
	            if (e >= 0) {
	              output(ExecPart(appender.substring(s, e)))
	              s = e + 1
	            }
	          } while (e != -1)
	          appender.delete(0, s)
	        }
	      } while (!eofreached) // && !channel.isEOF() && !channel.isClosed()) // => This old test is not good as data may remaining on the stream
          if (appender.size > 0) output(ExecPart(appender.toString()))
	      output(ExecEnd)
      } catch {
        case e:InterruptedIOException =>
          output(ExecTimeout)
        case e:InterruptedException =>
          output(ExecTimeout)
      }
    }
  }
  private object InputStreamThread {
    def apply(channel: ChannelExec, input: InputStream, output: ExecResult => Any) = {
      val newthread = new InputStreamThread(channel, input, output)
      newthread.start()
      newthread
    }
  }

}

// ==========================================================================================

class SSHScp(implicit ssh: SSH) extends TransfertOperations {

  override def get(remoteFilename: String): Option[String] = {
    getBytes(remoteFilename).map(new String(_, ssh.options.charset))
  }

  override def getBytes(remoteFilename: String): Option[Array[Byte]] = {
    var filesBuffer = Map.empty[String, ByteArrayOutputStream]
    def filename2outputStream(filename: String) = {
      val newout = new ByteArrayOutputStream()
      filesBuffer += filename -> newout
      newout
    }
    remoteFile2OutputStream(remoteFilename, filename2outputStream) match {
      case 0 => None
      case 1 => Some(filesBuffer.values.head.toByteArray)
      case _ => throw new RuntimeException("Want one file, but several files were found ! (%s)".format(filesBuffer.keys.mkString(",")))
    }
  }

  override def receive(remoteFilename: String, toLocalFile: File) {
    def filename2outputStream(filename: String) = new FileOutputStream(toLocalFile)
    remoteFile2OutputStream(remoteFilename, filename2outputStream) match {
      case 0 => throw new RuntimeException("Remote file name '%s' not found".format(remoteFilename))
      case 1 => // OK
      case _ => throw new RuntimeException("Want one file, but several files were found for '%s'".format(remoteFilename))
    }
  }

  override def put(data: String, remoteDestination: String) {
    putBytes(data.getBytes(ssh.options.charset), remoteDestination)
  }

  override def putBytes(data: Array[Byte], remoteDestination: String) {
    val sz = data.length
    val linput = new ByteArrayInputStream(data)
    val parts = remoteDestination.split("/")
    val rfilename = parts.last
    val rDirectory = if (parts.init.size == 0) "." else parts.init.mkString("/")

    inputStream2remoteFile(linput, sz, rfilename, rDirectory)
  }

  override def send(fromLocalFile: File, remoteDestination: String) {
    val sz = fromLocalFile.length
    val linput = new FileInputStream(fromLocalFile)
    val parts = remoteDestination.split("/", -1)
    val rfilename = if (parts.last.length == 0) fromLocalFile.getName else parts.last
    val rDirectory = if (parts.init.size == 0) "." else parts.init.mkString("/")

    inputStream2remoteFile(linput, sz, rfilename, rDirectory)
  }

  /**
   * upload a local input stream to a remote destination
   * @param localinput the input stream from which we read data
   * @param datasize amount of data to send (in bytes)
   * @param remoteFilename remote file name to use (just a filename, not a path, shouln't contain any path separator)
   * @param remoteDirectory remote destination directory for our file
   */

  def inputStream2remoteFile(
    localinput: InputStream,
    datasize: Long,
    remoteFilename: String,
    remoteDirectory: String) {
    val ch = ssh.jschsession.openChannel("exec").asInstanceOf[ChannelExec]
    try {
      ch.setCommand("""scp -p -t "%s"""".format(remoteDirectory))
      val sin = new BufferedInputStream(ch.getInputStream())
      val sout = ch.getOutputStream()
      ch.connect(ssh.options.connectTimeout.toInt)

      checkAck(sin)

      // send "C0644 filesize filename", where filename should not include '/'
      //println("******"+remoteFilename+" "+remoteDirectory)
      val command = "C0644 %d %s\n".format(datasize, remoteFilename) // TODO take into account remote file rights
      sout.write(command.getBytes("US-ASCII"))
      sout.flush()

      checkAck(sin)

      val bis = new BufferedInputStream(localinput)
      /*
      val chk = {
        var readCount=0L
        (x:Int) => {
          readCount+=1
          readCount <= datasize & x >= 0
        }
      }
      if (datasize>0) Stream.continually(bis.read()).takeWhile(chk(_)).foreach(sout.write(_))
      */
      var writtenBytes = 0
      while (writtenBytes < datasize) {
        val c = bis.read()
        if (c >= 0) {
          sout.write(c)
          writtenBytes += 1
        }
      }
      bis.close()

      // send '\0'
      sout.write(Array[Byte](0x00))
      sout.flush()

      checkAck(sin)

    } finally {
      if (ch.isConnected) ch.disconnect
    }
  }

  /**
   * lookup for remote files, for each found file send the content to
   * an OutputStream created using the specified builder
   * @param remoteFilenameMask file name or file mask
   * @return number of found files
   */

  def remoteFile2OutputStream(
    remoteFilenameMask: String,
    outputStreamBuilder: (String) => OutputStream): Int = {
    val ch = ssh.jschsession.openChannel("exec").asInstanceOf[ChannelExec]
    try {
      ch.setCommand("""scp -f "%s"""".format(remoteFilenameMask))
      val sin = new BufferedInputStream(ch.getInputStream())
      val sout = ch.getOutputStream()
      ch.connect(ssh.options.connectTimeout.toInt)

      sout.write(0)
      sout.flush()

      var count = 0
      val buf = new StringBuilder() // Warning : Mutable state, take care
      def bufAppend(x: Int) { buf.append(x.asInstanceOf[Char]) }
      def bufReset() { buf.setLength(0) }
      def bufStr = buf.toString

      while (checkAck(sin) == 'C') {
        val fileRights = new Array[Byte](5)
        sin.read(fileRights, 0, 5)

        bufReset()
        Stream.continually(sin.read()).takeWhile(_ != ' ').foreach(bufAppend(_))
        val fz = bufStr.toLong

        bufReset()
        Stream.continually(sin.read()).takeWhile(_ != 0x0a).foreach(bufAppend(_))
        val filename = bufStr

        //println(remoteFilenameMask+ " " + count + " " + new String(fileRights)+ " '"+ filename + "' #" + fz)

        sout.write(0)
        sout.flush()

        val fos = new BufferedOutputStream(outputStreamBuilder(filename), 8192)

        /*
        val chk = {
          var readCount=0L
          (x:Int) => {
            readCount+=1
            readCount <= fz && x >= 0
          }
        }
        if (fz>0) Stream.continually(sin.read()).takeWhile(chk(_)).foreach(fos.write(_))
        */

        var writtenBytes = 0L
        while (writtenBytes < fz) {
          val c = sin.read()
          if (c >= 0) {
            fos.write(c)
            writtenBytes += 1
          }
        }

        fos.close

        count += 1

        checkAck(sin)
        sout.write(0)
        sout.flush()
      }

      count
    } finally {
      if (ch.isConnected) ch.disconnect
    }
  }

  private def checkAck(in: InputStream): Int = {
    def consumeMessage() = {
      val sb = new StringBuffer()
      Stream.continually(in.read())
        .takeWhile(x => (x != '\n') && (x != -1))
        .foreach(x => sb.append(x.asInstanceOf[Char]))
    }
    in.read() match {
      case 1 => throw new RuntimeException("SSH transfert protocol error " + consumeMessage())
      case 2 => throw new RuntimeException("SSH transfert protocol fatal error " + consumeMessage())
      case x => x
    }
  }

  def close() {}

}

// ==========================================================================================

class SSHFtp(implicit ssh: SSH) extends TransfertOperations {
  private val channel: ChannelSftp = {
    //jschftpchannel.connect(link.connectTimeout)
    val ch = ssh.jschsession.openChannel("sftp").asInstanceOf[ChannelSftp]
    ch.connect(ssh.options.connectTimeout.toInt)
    ch
  }

  def close() = {
    channel.quit
    channel.disconnect
  }

  override def get(filename: String): Option[String] = {
    try {
      implicit val codec = new io.Codec(Charset.forName(ssh.options.charset))
      Some(new BufferedSource(channel.get(filename)).mkString)
    } catch {
      case e: SftpException if (e.id == 2) => None // File doesn't exist
      case e: IOException => None
    }
  }

  override def getBytes(filename: String): Option[Array[Byte]] = {
    try {
      Some(SSHTools.inputStream2ByteArray(channel.get(filename)))
    } catch {
      case e: SftpException if (e.id == 2) => None // File doesn't exist
      case e: IOException => None
    }
  }

  override def receive(remoteFilename: String, localFile: File) {
    try {
      channel.get(remoteFilename, new FileOutputStream(localFile))
    } catch {
      case e: SftpException if (e.id == 2) => None // File doesn't exist
      case e: IOException => None
    }
  }

  override def put(data: String, remoteFilename: String) {
    channel.put(new ByteArrayInputStream(data.getBytes(ssh.options.charset)), remoteFilename)
  }

  override def putBytes(data: Array[Byte], remoteFilename: String) {
    channel.put(new ByteArrayInputStream(data), remoteFilename)
  }

  override def send(localFile: File, remoteFilename: String) {
    channel.put(new FileInputStream(localFile), remoteFilename)
  }

}

// ==========================================================================================

class SSHShell(implicit ssh: SSH) extends ShellOperations {
  val readyMessage = "ready-" + System.currentTimeMillis()
  val defaultPrompt = """-PRMT-: """
  val customPromptGiven = ssh.options.prompt.isDefined
  val prompt = ssh.options.prompt getOrElse defaultPrompt

  val options = ssh.options

  val (channel, toServer, fromServer) = {
    var ch: ChannelShell = ssh.jschsession.openChannel("shell").asInstanceOf[ChannelShell]
    ch.setPtyType("dumb")
    ch.setXForwarding(false)
    //ch.setEnv("COLUMNS", "500") // Can't be use, by default PermitUserEnvironment=no in sshd_config 

    val pos = new PipedOutputStream()
    val pis = new PipedInputStream(pos)
    val toServer = new Producer(pos)
    ch.setInputStream(pis)

    val fromServer = new ConsumerOutputStream(customPromptGiven) // if the customPrompt is given, we consider we're ready to send/receive commands
    ch.setOutputStream(fromServer)

    ch.connect(ssh.options.connectTimeout.toInt)

    (ch, toServer, fromServer)
  }

  def close() = {
    fromServer.close()
    toServer.close()
    channel.disconnect()
  }

  override def execute(cmd: SSHCommand): String = {
    sendCommand(cmd.cmd)
    fromServer.getResponse()
  }

  override def executeAll(cmds: SSHBatch): Iterable[String] = cmds.cmdList.map(execute(_))

  /*
  def execute[I <: Iterable[String]](commands: I)(implicit bf: CanBuildFrom[I, String, I]): I = {
    var builder = bf.apply()
    try {
      for (cmd <- commands) builder += (execute(cmd))
    }
    builder.result
  }
*/

  private var doInit = true
  private def sendCommand(cmd: String): Unit = {
    if (doInit) {
      if (ssh.options.prompt.isEmpty) {
        // if no prompt is given we assume that a standard sh/bash/ksh shell is used
        toServer.sendCommand("unset LS_COLORS")
        toServer.sendCommand("unset EDITOR")
        toServer.sendCommand("unset PAGER")
        toServer.sendCommand("COLUMNS=500")
        toServer.sendCommand("PS1='%s'".format(defaultPrompt))
        //toServer.sendCommand("set +o emacs")  // => Makes everything not working anymore, JSCH problem ?
        //toServer.sendCommand("set +o vi") // => Makes everything not working anymore, JSCH problem ?
        toServer.sendCommand("echo '%s'".format(readyMessage)) // ' are important to distinguish between the command and the result
        fromServer.waitReady()
        fromServer.getResponse() // ready response
      } else {
        fromServer.waitReady()
        fromServer.getResponse() // For the initial prompt
      }
      doInit = false
    }
    toServer.sendCommand(cmd)
  }

  class Producer(output: OutputStream) {
    def sendCommand(cmd: String) {
      output.write(cmd.getBytes)
      output.write("\n".getBytes)
      output.flush()
    }
    def close() { output.close() }
  }

  class ConsumerOutputStream(checkReady: Boolean) extends OutputStream {

    private val resultsQueue = new ArrayBlockingQueue[String](100)

    def now = System.currentTimeMillis()

    def hasResponse() = resultsQueue.size > 0

    def getResponse(timeout: Long = ssh.options.timeout) = {
      val started = now
      resultsQueue.take()
    }

    private var ready = checkReady
    private val readyQueue = new ArrayBlockingQueue[String](1)
    def waitReady() {
      //Thread.sleep(500) // TODO : Bad but Mandatory to get some response from JSCH => Find a better way
      if (ready == false) readyQueue.take()
    }
    private val readyMessageQuotePrefix="'"+readyMessage
    private val promptEqualPrefix="="+prompt

    private val consumerAppender = new StringBuilder(8192)
    private var searchForPromptIndex = 0
    private val promptSize = prompt.size
    private var lastPromptChar = prompt.last
    def write(b: Int) {
      if (b != 13) { //CR removed... CR is always added by JSCH !!!!
        val ch=b.toChar
        consumerAppender.append(ch) // TODO - Add charset support
        if (!ready) { // We want the response and only the response, not the echoed command, that's why the quote is prefixed
          if ( consumerAppender.endsWith(readyMessage) && 
              !consumerAppender.endsWith(readyMessageQuotePrefix)) {
            // wait for at least some results, will tell us that the ssh cnx is ready
            ready = true
            readyQueue.put("ready")
          }
        } else if (lastPromptChar==ch && consumerAppender.endsWith(prompt) && !consumerAppender.endsWith(promptEqualPrefix)) {
          val promptIndex = consumerAppender.size - promptSize
          val firstNlIndex = consumerAppender.indexOf("\n")
          val result = consumerAppender.substring(firstNlIndex + 1, promptIndex)
          resultsQueue.put(result)
          searchForPromptIndex = 0
          consumerAppender.clear
        } else {
          searchForPromptIndex = consumerAppender.size - promptSize
          if (searchForPromptIndex < 0) searchForPromptIndex = 0
        }
      }
    }
  }

}
// ==========================================================================================

/* Attention
 * - L'option PasswordAuthentication doit être à "yes" sinon impossible de s'authentifier
 *   (Configuration au niveau du serveur SSH) SSI on n'implemente pas "promptKeyboardInteractive"
 *
 */
case class SSHUserInfo(password: Option[String] = None, passphrase: Option[String] = None) extends UserInfo with UIKeyboardInteractive {
  override def getPassphrase() = passphrase getOrElse ""
  override def getPassword() = password getOrElse ""
  override def promptPassword(message: String) = true
  override def promptPassphrase(message: String) = true
  override def promptYesNo(message: String) = true
  override def showMessage(message: String) = {}
  override def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = Array(getPassword())
}

// ==========================================================================================

object SSHTools {
  def md5sum(str: String): String = {
    md5sum(new ByteArrayInputStream(str.getBytes())) // TODO : Warning manage charsets...
  }
  def md5sum(input: InputStream): String = {
    val bis = new BufferedInputStream(input)
    val buf = new Array[Byte](1024)
    val md5 = java.security.MessageDigest.getInstance("MD5")
    Stream.continually(bis.read(buf)).takeWhile(_ != -1).foreach(md5.update(buf, 0, _))
    md5.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft("") { _ + _ }
  }
  def getFile(filename: String): String = {
    new BufferedSource(new FileInputStream(filename)).mkString
  }
  def getRawFile(filename: String): Array[Byte] = {
    inputStream2ByteArray(new FileInputStream(filename))
  }
  def inputStream2ByteArray(input: InputStream): Array[Byte] = {
    val fos = new ByteArrayOutputStream(65535)
    val bfos = new BufferedOutputStream(fos, 16384)
    val bis = new BufferedInputStream(input)
    val buffer = new Array[Byte](8192)
    try {
      Stream.continually(bis.read(buffer))
        .takeWhile(_ != -1)
        .foreach(bfos.write(buffer, 0, _))
    } finally {
      bfos.close
      fos.close
    }
    fos.toByteArray
  }
  def basename(name: String, ext: String) = if (name contains ext) name.substring(0, name.indexOf(ext)) else name

}
