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
import scala.actors._
import scala.actors.Actor._
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
import java.util.Date
import java.text.SimpleDateFormat

trait SSHAutoClose {
  // Automatic resource liberation
  def usingSSH[T <: { def close() }, R](resource: T)(block: T => R) = {
    try block(resource)
    finally resource.close
  }
}

/**
  * SSHCommand class models ssh command
  * @author David Crosson
  */
class SSHCommand(val cmd: String) {
  def §§(implicit ssh: SSH) = ssh.shell { _ execute cmd }
}

/**
  * SSHCommand object implicit conversions container  
  * @author David Crosson
  */
object SSHCommand {
  implicit def stringToCommand(cmd: String) = new SSHCommand(cmd)
}

/**
  * SSHBatch class models ssh batch (in fact a list of commands)  
  * @author David Crosson
  */
class SSHBatch(val cmdList: List[String]) {
  def §§(implicit ssh: SSH) = ssh.shell { _ execute cmdList }
}

/**
  * SSHBatch object implicit conversions container  
  * @author David Crosson
  */
object SSHBatch {
  implicit def stringListToBatchList(cmdList: List[String]) = new SSHBatch(cmdList)
}


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
  implicit def string2password(pass: String) = new SSHPassword(Some(pass))
  implicit def stringOpt2password(passopt: Option[String]) = new SSHPassword(passopt)
}

/**
  * SSHOptions stores all ssh parameters
  * @author David Crosson
  */
case class SSHOptions(
  host: String = "localhost",
  username: String = util.Properties.userName,
  password: SSHPassword = NoPassword,
  passphrase: SSHPassword = NoPassword,
  name: Option[String] = None,
  port: Int = 22,
  prompt: Option[String] = None,
  timeout: Long = 300000,
  connectTimeout: Long = 30000,
  retryCount: Int = 5,
  retryDelay: Int = 2000,
  sshUserDir: String = SP.userHome + FS + ".ssh",
  charset: String = "ISO-8859-15")

  
/**
  * SSH object factories
  * @author David Crosson
  */
object SSH extends SSHAutoClose {

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
    timeout: Int = 300000)(withssh: (SSH) => T): T = usingSSH(new SSH(SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))) {
    withssh(_)
  }
  /**
    * Executes the given code then closes the new ssh associated session. 
    * @param options ssh options
    * @param withssh code bloc to execute
    * @return "withssh" returns type
    */
  def once[T](options: SSHOptions)(withssh: (SSH) => T) = usingSSH(new SSH(options)) {
    withssh(_)
  }
  /**
    * Executes the given code then closes the new ssh associated session. 
    * @param someOptions Some ssh options or None, if None is given, nothing will be done 
    * @param withssh code bloc to execute
    * @return "withssh" returns type
    */
  def once[T](someOptions: Option[SSHOptions])(withssh: (SSH) => Option[T]) = someOptions map { options =>
    usingSSH(new SSH(options)) {
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
    timeout: Int = 300000)(withsh: (SSHShell) => T): T = shell[T](SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))(withsh)

  /**
    * Executes the given code then closes the new ssh shell associated session. 
    * @param options ssh options
    * @param withsh code bloc to execute
    * @return "withssh" returns type
    */
  def shell[T](options: SSHOptions)(withsh: (SSHShell) => T): T = usingSSH(new SSH(options)) { ssh =>
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
    timeout: Int = 300000)(withftp: (SSHFtp) => T): T = ftp[T](SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))(withftp)

  /**
    * Executes the given code then closes the new sftp associated session. 
    * @param options ssh options
    * @param withftp code bloc to execute
    * @return "withftp" returns type
    */
  def ftp[T](options: SSHOptions)(withftp: (SSHFtp) => T): T = usingSSH(new SSH(options)) { ssh =>
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
    timeout: Int = 300000)(withshftp: (SSHShell, SSHFtp) => T): T = shellAndFtp[T](SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))(withshftp)

  /**
    * Executes the given code then closes the new ssh shell and sftp associated sessions. 
    * @param options ssh options
    * @param withshftp code bloc to execute
    * @return "withshftp" returns type
    */
  def shellAndFtp[T](options: SSHOptions)(withshftp: (SSHShell, SSHFtp) => T): T = usingSSH(new SSH(options)) { ssh =>
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
    timeout: Int = 300000) = new SSH(SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))

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

}




/**
  * SSH class. This class is the main entry point to the API
  * @author David Crosson
  */
class SSH(val options: SSHOptions) extends SSHAutoClose {
  private implicit val ssh = this
  private val jsch = new JSch
  val jschsession: Session = {
    val idrsa = new File(options.sshUserDir, "id_rsa")
    val iddsa = new File(options.sshUserDir, "id_dsa")
    if (idrsa.exists) jsch.addIdentity(idrsa.getAbsolutePath)
    if (iddsa.exists) jsch.addIdentity(iddsa.getAbsolutePath)
    val ses = jsch.getSession(options.username, options.host, options.port)
    ses.setTimeout(options.timeout.toInt)  // Socket timeout
    ses setUserInfo SSHUserInfo(options.password.password, options.passphrase.password)
    ses.connect(options.connectTimeout.toInt)
    ses
  }

  //def apply[T](proc: (SSH) => T) = proc(this)  => Removed because it doesn't deal with close...

  def shell[T](proc: (SSHShell) => T) = usingSSH(new SSHShell) { proc(_) }

  def ftp[T](proc: (SSHFtp) => T) = usingSSH(new SSHFtp) { proc(_) }

  def noerr(data: Option[String]) {}

  def run(cmd: String, out: Option[String] => Any, err: Option[String] => Any = noerr) = new SSHExec(cmd, out, err)

  def execute(cmd: SSHCommand) =
    //shell { _ execute cmd.cmd }    // Using SSHShell channel  (lower performances)
    execOnce(cmd) // Using SSHExec channel (better performances)

  def executeAndTrim(cmd: SSHCommand) = execute(cmd).trim()

  def executeAndTrimSplit(cmd: SSHCommand) = execute(cmd).trim().split("\r?\n")

  def execute(cmds: SSHBatch) = shell { _ execute cmds.cmdList }

  def executeAndTrim(cmds: SSHBatch) = execute(cmds.cmdList) map { _.trim }

  def executeAndTrimSplit(cmds: SSHBatch) = execute(cmds.cmdList) map { _.trim.split("\r?\n") }

  def execOnceAndTrim(scmd: SSHCommand) = execOnce(scmd).trim()

  def execOnce(scmd: SSHCommand) = {
    val sb = new StringBuilder()
    def recvStandardOutput(content: Option[String]) {
      for (c <- content) {
        if (sb.size > 0) sb.append("\n")
        sb.append(c)
      }
    }
    var runner: Option[SSHExec] = None
    try {
      runner = Some(new SSHExec(scmd.cmd, recvStandardOutput, _ => None))
      runner foreach { _.waitForEnd }
    } finally {
      runner foreach { _.close }
    }
    sb.toString()
  }

  def get(remoteFilename: String) = ssh.ftp { _ get remoteFilename }

  def getBytes(remoteFilename: String) = ssh.ftp { _ getBytes remoteFilename }

  def put(data: String, remoteFilename: String) = ssh.ftp { _ put (data, remoteFilename) }

  def receive(remoteFilename: String, toLocalFilename: String) = ssh.ftp { _.receive(remoteFilename, toLocalFilename) }

  def send(fromLocalFilename: String, remoteFilename: String) = ssh.ftp {
    _.send(fromLocalFilename, remoteFilename)
  }

  def newShell = new SSHShell

  def newSftp = new SSHFtp

  def close() { jschsession.disconnect }

  // Now a set of tools

  /**
   * Remote file size in bytes
   * @param filename file name
   * @return optional file size, or None if filename was not found
   */
  def fileSize(filename: String):Option[Long] = shell { _.fileSize(filename)}

  /**
   * Remote file md5sum
   * @param filename file name
   * @return md5sum as an optional String, or None if filename was not found
   */
  def md5sum(filename: String):Option[String] = shell { _.md5sum(filename)}
  
  /**
   * Remote file sha1sum
   * @param filename file name
   * @return sha1sum as an optional String, or None if filename was not found
   */
  def sha1sum(filename: String):Option[String] = shell { _.sha1sum(filename)}
  
  /**
   * *nix system name (Linux, AIX, SunOS, ...)
   * @return remote *nix system name
   */
  lazy val uname:String = shell { _.uname}

  /**
   * List files in specified directory
   * @return current directory files as an Iterable
   */
  def ls():Iterable[String] = shell {_.ls()}
  
  /**
   * List files in specified directory
   * @param dirname directory to look into
   * @return current directory files as an Iterable
   */
  def ls(dirname:String):Iterable[String] = shell {_.ls(dirname)}
  
  /**
   * Get current working directory
   * @return current directory
   */
  def pwd():String = shell { _.pwd()}

  /**
   * Get remote host name
   * @return host name
   */
  lazy val hostname:String = shell { _.hostname}
  
  /**
   * Get remote date, as a java class Date instance (minimal resolution = 1 second)
   * @return The remote system current date as a java Date class instance
   */
  def date():Date = shell { _.date()}
  
  /**
   * Find file modified after the given date (Warning, minimal resolution = 1 minute) 
   * @param root Search for file from this root directory
   * @param after Date parameter
   * @return list of paths (relative to root) modified after the specified date
   */
  def findAfterDate(root:String, after:Date):Iterable[String] = shell {_.findAfterDate(root, after)}
  
  /**
   * Generic test (man test, for arguments)
   * @param that condition
   * @return True if condition is met
   */
  def test(that:String):Boolean = shell {_.test(that)}
  
  /**
   * Does specified filename exist ?
   * @param filename file name
   * @return True if file exists
   */
  def exists(filename:String):Boolean = shell {_.exists(filename)}
  
  /**
   * Is file name a directory
   * @param filename file name
   * @return True if file is a directory
   */
  def isDirectory(filename:String):Boolean = shell {_.isDirectory(filename)}
  
  /**
   * Is file name a regular file
   * @param filename file name
   * @return True if file is a regular file
   */
  def isFile(filename:String):Boolean = shell {_.isFile(filename)}
  
  /**
   * Is filename executable ?
   * @param filename file name
   * @return True if file is executable
   */
  def isExecutable(filename:String):Boolean = shell {_.isExecutable(filename)}
}



class SSHExec(cmd: String, out: Option[String] => Any, err: Option[String] => Any)(implicit ssh: SSH) {

  private val (channel, stdout, stderr, stdin) = {
    val ch = ssh.jschsession.openChannel("exec").asInstanceOf[ChannelExec]
    ch.setCommand(cmd.getBytes())
    val stdout = ch.getInputStream()
    val stderr = ch.getErrStream()
    val stdin = ch.getOutputStream()
    ch.connect(ssh.options.connectTimeout.toInt)
    (ch, stdout, stderr, stdin)
  }
  private val stdoutThread = InputStreamThread(channel, stdout, out)
  private val stderrThread = InputStreamThread(channel, stderr, err)

  def giveInputLine(line: String) {
    stdin.write(line.getBytes())
    stdin.write("\n".getBytes())
    stdin.flush()
  }

  def waitForEnd {
    stdoutThread.join()
    stderrThread.join()
    close()
  }

  def close() = {
    stdin.close()
    stdoutThread.interrupt()
    stderrThread.interrupt()
    channel.disconnect
  }

  private class InputStreamThread(channel: ChannelExec, input: InputStream, output: Option[String] => Any) extends Thread {
    override def run() {
      val bufsize = 16 * 1024
      val charset = Charset.forName(ssh.options.charset)
      val binput = new BufferedInputStream(input)
      val bytes = Array.ofDim[Byte](bufsize)
      val buffer = ByteBuffer.allocate(bufsize)
      val appender = new StringBuilder()
      var eofreached = false
      do {
        //val available = binput.available()
        //if (available ==0) {
        //Thread.sleep(100) 
        //} /*else {*/  
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
              output(Some(appender.substring(s, e)))
              s = e + 1
            }
          } while (e != -1)
          appender.delete(0, s)
          //}
        }
      } while (!eofreached) // && !channel.isEOF() && !channel.isClosed()) // => This old test is not good as data may remaining on the stream
      if (appender.size > 0) output(Some(appender.toString()))
      output(None)
    }
  }
  private object InputStreamThread {
    def apply(channel: ChannelExec, input: InputStream, output: Option[String] => Any) = {
      val newthread = new InputStreamThread(channel, input, output)
      newthread.start()
      newthread
    }
  }

}

class SSHFtp(implicit ssh: SSH) {
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

  def get(filename: String): Option[String] = {
    try {
      Some(new BufferedSource(channel.get(filename)).mkString)
    } catch {
      case e: SftpException if (e.id == 2) => None // File doesn't exist
      case e: IOException => None
    }
  }
  def put(data: String, remoteFilename: String) {
    channel.put(new ByteArrayInputStream(data.getBytes), remoteFilename)
  }

  def receive(remoteFilename: String, localFile: File) = {
    try {
      channel.get(remoteFilename, new FileOutputStream(localFile))
    } catch {
      case e: SftpException if (e.id == 2) => None // File doesn't exist
      case e: IOException => None
    }
  }
  def receive(remoteFilename: String, localFilename: String) = {
    try {
      channel.get(remoteFilename, new FileOutputStream(localFilename))
    } catch {
      case e: SftpException if (e.id == 2) => None // File doesn't exist
      case e: IOException => None
    }
  }
  def send(localFilename: String, remoteFilename: String) = {
    channel.put(new FileInputStream(localFilename), remoteFilename)
  }

  def getBytes(filename: String): Option[Array[Byte]] = {
    try {
      Some(SSHTools.inputStream2ByteArray(channel.get(filename)))
    } catch {
      case e: SftpException if (e.id == 2) => None // File doesn't exist
      case e: IOException => None
    }
  }

}

class SSHShell(implicit ssh: SSH) {
  val readyMessage = "ready-" + System.currentTimeMillis()
  val defaultPrompt = """-PRMT-: """
  val customPromptGiven = ssh.options.prompt.isDefined
  val prompt = ssh.options.prompt getOrElse defaultPrompt

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

  def execute[I <: Iterable[String]](commands: I)(implicit bf: CanBuildFrom[I, String, I]): I = {
    var builder = bf.apply()
    try {
      for (cmd <- commands) builder += (execute(cmd))
    }
    builder.result
  }

  def close() = {
    fromServer.close()
    toServer.close()
    channel.disconnect()
  }

  def executeAndContinue(cmd: String, cont: String => Unit): Unit = cont(execute(cmd))

  def executeAndTrim(cmd: String): String = execute(cmd).trim()

  def executeAndTrimSplit(cmd: String): Array[String] = execute(cmd).trim().split("\r?\n")

  def execute(cmd: String, timeout: Long = ssh.options.timeout): String = {
    sendCommand(cmd)
    fromServer.getResponse()
  }

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

    private val consumerAppender = new StringBuilder(8192)
    private var searchForPromptIndex = 0
    private val promptSize = prompt.size
    private var lastPromptChar = prompt.last
    def write(b: Int) {
      if (b != 13) { //CR removed... CR is always added by JSCH !!!!
        consumerAppender.append(b.toChar) // TODO - Add charset support
        if (!ready) { // We want the response and only the response, not the echoed command, thats's why the quote is prefixed
          if (consumerAppender.endsWith(readyMessage) && !consumerAppender.endsWith("'" + readyMessage)) {
            // wait for at least some results, will tell us that the ssh cnx is ready
            ready = true
            readyQueue.put("ready")
          }
        } else if (consumerAppender.endsWith(prompt)) {
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

  // Now a set of useful method for standard *nix systems

  private def genoptcmd(cmd:String):Option[String] = {
    executeAndTrim("""%s 2>/dev/null""".format(cmd)) match {
      case "" => None
      case str => Some(str)
    }    
  }
  
  /**
   * Remote file size in bytes
   * @param filename file name
   * @return optional file size, or None if filename was not found
   */
  def fileSize(filename: String):Option[Long] = 
    genoptcmd("""ls -ld "%s" """.format(filename)).map(_.split("""\s+""")(4).toLong)  


  /**
   * Remote file md5sum
   * @param filename file name
   * @return md5sum as an optional String, or None if filename was not found
   */
  def md5sum(filename: String):Option[String] =
    genoptcmd("""md5sum "%s" """.format(filename)).map(_.split("""\s+""")(0))  

  
  /**
   * Remote file sha1sum
   * @param filename file name 
   * @return sha1sum as an optional String, or None if filename was not found
   */
  def sha1sum(filename: String):Option[String] =
    genoptcmd("""sha1sum "%s" """.format(filename)).map(_.split("""\s+""")(0))  

  
  /**
   * *nix system name (Linux, AIX, SunOS, ...)
   * @return remote *nix system name
   */
  lazy val uname:String = executeAndTrim("""uname 2>/dev/null""")
  
  /**
   * List files in specified directory
   * @return current directory files as an Iterable
   */
  def ls():Iterable[String] = ls(".")

  /**
   * List files in specified directory
   * @param dirname directory to look into
   * @return current directory files as an Iterable
   */
  def ls(dirname:String):Iterable[String] = executeAndTrimSplit("""ls --format=single-column "%s" """.format(dirname))
  
  /**
   * Get current working directory
   * @return current directory
   */
  def pwd():String = executeAndTrim("""pwd""")
  
  /**
   * Change current working directory to home directory
   */
  def cd() {execute("cd")}
  
  /**
   * Change current working directory to the specified directory
   * @param dirname directory name
   */
  def cd(dirname:String) {execute("""cd "%s" """.format(dirname))}
  
  /**
   * Get remote host name
   * @return host name
   */
  lazy val hostname:String = executeAndTrim("""hostname""")

  /**
   * Get remote date, as a java class Date instance (minimal resolution = 1 second)
   * @return The remote system current date as a java Date class instance
   */
  def date():Date = {
    val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z")
    val d = executeAndTrim("date '+%Y-%m-%d %H:%M:%S %z'")
    sdf.parse(d)
  }
  
  /**
   * Find file modified after the given date (Warning, minimal resolution = 1 minute) 
   * @param root Search for file from this root directory
   * @param after Date parameter
   * @return list of paths (relative to root) modified after the specified date
   */
  def findAfterDate(root:String, after:Date):Iterable[String] = {
    def ellapsedInMn(thatDate:Date):Long =  (date().getTime - thatDate.getTime)/1000/60
    //deprecated : // def ellapsedInMn(thatDate:Date):Long =  (new Date().getTime - thatDate.getTime)/1000/60
    val findpattern = uname.toLowerCase match {
	    case "linux"|"aix" => """find %s -follow -type f -mmin '-%d' 2>/dev/null"""   // "%s" => %s to enable file/dir patterns
	    case "sunos" => throw new RuntimeException("SunOS not supported - find command doesn't support -mmin parameter")
	    case _       => """find %s -type f -mmin '-%d' 2>/dev/null"""
    }
    val findcommand = findpattern.format(root, ellapsedInMn(after))
    executeAndTrimSplit(findcommand)
  }

  
  /**
   * Generic test usage
   */
  private def testFile(testopt:String, filename:String):Boolean = {
    val cmd = """test %s "%s" ; echo $?""".format(testopt, filename)
    executeAndTrim(cmd).toInt == 0
  }

  /**
   * Generic test (man test, for arguments)
   * @param that condition
   * @return True if condition is met
   */
  def test(that:String):Boolean = {
    val cmd = """test %s ; echo $?""".format(that)
    executeAndTrim(cmd).toInt == 0
  }
  
  /**
   * Does specified filename exist ?
   * @param filename file name
   * @return True if file exists
   */
  def exists(filename:String):Boolean = testFile("-e", filename)

  /**
   * Is file name a directory
   * @param filename file name
   * @return True if file is a directory
   */
  def isDirectory(filename:String):Boolean = testFile("-d", filename)
  
  /**
   * Is file name a regular file
   * @param filename file name
   * @return True if file is a regular file
   */
  def isFile(filename:String):Boolean = testFile("-f", filename)
  
  /**
   * Is filename executable ?
   * @param filename file name
   * @return True if file is executable
   */
  def isExecutable(filename:String):Boolean = testFile("-x", filename)
}


// =============================================================================

/* Attention
 * - L'option PasswordAuthentication doit être à "yes" sinon impossible de s'authentifier
 *   (Configuration au niveau du serveur SSH) SSI on n'implemente pas "promptKeyboardInteractive"
 *
 */
case class SSHUserInfo(password: Option[String] = None, passphrase: Option[String] = None) extends UserInfo with UIKeyboardInteractive {
  override def getPassphrase() = passphrase getOrElse ""
  override def getPassword() = password getOrElse ""
  override def promptPassword(message: String) = true
  override def promptPassphrase(message: String) = false
  override def promptYesNo(message: String) = true
  override def showMessage(message: String) = {}
  override def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = Array(getPassword())
}

// =============================================================================

object SSHTools {
  def md5sum(str:String):String = {
    md5sum(new ByteArrayInputStream(str.getBytes())) // TODO : Warning manage charsets...
  }
  def md5sum(input:InputStream):String = {
    val bis = new BufferedInputStream(input)
    val buf = new Array[Byte](1024)
    val md5 = java.security.MessageDigest.getInstance("MD5")
    Stream.continually(bis.read(buf)).takeWhile(_ != -1).foreach(md5.update(buf, 0, _))
    md5.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
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
    val buffer = new Array[Byte](1024)
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
