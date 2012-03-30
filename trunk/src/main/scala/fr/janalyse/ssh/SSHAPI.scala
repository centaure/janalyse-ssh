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

trait SSHAutoClose {
  // Automatic resource liberation
  def usingSSH[T <: { def close() }, R](resource: T)(block: T => R) = {
    try block(resource)
    finally resource.close
  }
}

class SSHCommand(val cmd: String) {
  def !(implicit ssh: SSH) = ssh.shell { _ execute cmd }
}

object SSHCommand {
  implicit def toCommand(cmd: String) = new SSHCommand(cmd)
}

class SSHBatch(val cmdList: List[String]) {
  def !(implicit ssh: SSH) = ssh.shell { _ batch cmdList }
}

object SSHBatch {
  implicit def toBatchList(cmdList: List[String]) = new SSHBatch(cmdList)
}

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

object SSHRemoteFile {
  implicit def toRemoteFile(filename: String) = new SSHRemoteFile(filename)
}

/*
trait OutputMessage {
  val line: String
}
case class StandardOutputMessage(line: String) extends OutputMessage
case class StandardErrorMessage(line: String) extends OutputMessage
case class StandardOutputClosed(line:String="Finished") extends OutputMessage 
case class StandardErrorClosed(line:String="Finished") extends OutputMessage
*/

case class SSHOptions(
  username: String = util.Properties.userName,
  password: Option[String] = None,
  passphrase: Option[String] = None,
  host: String = "localhost",
  port: Int = 22,
  prompt: Option[String] = None,
  timeout: Long = 30000,
  retryCount: Int = 5,
  retryDelay: Int = 2000,
  sshUserDir: String = SP.userHome + FS + ".ssh",
  charset: String = "ISO-8859-15")

object SSH extends SSHAutoClose {

  def connect[T](
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000)(withssh: (SSH) => T): T = usingSSH(new SSH(SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout))) {
    withssh(_)
  }
  def connect[T](options: SSHOptions)(withssh: (SSH) => T) = usingSSH(new SSH(options)) {
    withssh(_)
  }
  def connect[T](someOptions: Option[SSHOptions])(withssh: (SSH) => Option[T]) = someOptions map { options =>
    usingSSH(new SSH(options)) {
      withssh(_)
    }
  }

  def shell[T](
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000)(withsh: (SSHShell) => T): T = shell[T](SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout))(withsh)

  def shell[T](options: SSHOptions)(withsh: (SSHShell) => T): T = usingSSH(new SSH(options)) { ssh =>
    ssh.shell { sh => withsh(sh) }
  }
  def shell[T](someOptions: Option[SSHOptions])(withsh: (SSHShell) => T): Option[T] = someOptions map { shell[T](_)(withsh) }

  def ftp[T](
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000)(withftp: (SSHFtp) => T): T = ftp[T](SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout))(withftp)

  def ftp[T](options: SSHOptions)(withftp: (SSHFtp) => T): T = usingSSH(new SSH(options)) { ssh =>
    ssh.ftp { ftp => withftp(ftp) }
  }
  def ftp[T](someOptions: Option[SSHOptions])(withftp: (SSHFtp) => T): Option[T] = someOptions map { ftp[T](_)(withftp) }

  def shellAndFtp[T](
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000)(withshftp: (SSHShell, SSHFtp) => T): T = shellAndFtp[T](SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout))(withshftp)

  def shellAndFtp[T](options: SSHOptions)(withshftp: (SSHShell, SSHFtp) => T): T = usingSSH(new SSH(options)) { ssh =>
    ssh.shell { sh => ssh.ftp { ftp => withshftp(sh, ftp) } }
  }
  def shellAndFtp[T](someOptions: Option[SSHOptions])(withshftp: (SSHShell, SSHFtp) => T): Option[T] = someOptions map { shellAndFtp[T](_)(withshftp) }

  def apply(
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000) = new SSH(SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout))

  def apply(options: SSHOptions) = new SSH(options)
  def apply(someOptions: Option[SSHOptions]): Option[SSH] = someOptions map { new SSH(_) }

  //implicit def toCommand(cmd: String) = new SSHCommand(cmd)
  //implicit def toBatchList(cmdList: List[String]) = new SSHBatch(cmdList)
  //implicit def toRemoteFile(filename: String) = new SSHRemoteFile(filename)
}

class SSH(val options: SSHOptions) extends SSHAutoClose {
  private implicit val ssh = this
  private val jsch = new JSch
  var jschsession: Session = initSession

  def apply[T](proc: (SSH) => T) = proc(this)

  def shell[T](proc: (SSHShell) => T) = usingSSH(new SSHShell) { proc(_) }

  def ftp[T](proc: (SSHFtp) => T) = usingSSH(new SSHFtp) { proc(_) }

  def noerr(data: Option[String]) {}

  def run(cmd: String, out: Option[String] => Any, err: Option[String] => Any = noerr) = new SSHExec(cmd, out, err)

  def execute(cmd: SSHCommand) = shell { _ execute cmd.cmd }

  def execute(cmds: SSHBatch) = shell { _ batch cmds.cmdList }

  def executeAndTrim(cmd: SSHCommand) = execute(cmd).trim()

  def executeAndTrimSplit(cmd: SSHCommand) = execute(cmd).trim().split("\r?\n")

  def executeAndTrim(cmds: SSHBatch) = execute(cmds.cmdList) map { _.trim }

  def executeAndTrimSplit(cmds: SSHBatch) = execute(cmds.cmdList) map { _.trim.split("\r?\n") }

  def get(remoteFilename: String) = ssh.ftp { _ get remoteFilename }

  def getBytes(remoteFilename: String) = ssh.ftp { _ getBytes remoteFilename }

  def put(data: String, remoteFilename: String) = ssh.ftp { _ put (data, remoteFilename) }

  def receive(remoteFilename: String, toLocalFilename: String) = ssh.ftp { _.receive(remoteFilename, toLocalFilename) }

  def send(fromLocalFilename: String, remoteFilename: String) = ssh.ftp {
    _.send(fromLocalFilename, remoteFilename)
  }

  def newShell = new SSHShell

  def newSftp = new SSHFtp

  def close() {
    if (jschsession != null) {
      jschsession.disconnect
      jschsession = null
    }
  }

  private def initSession = {
    if (jschsession == null || !jschsession.isConnected) {
      close
      val idrsa = new File(options.sshUserDir, "id_rsa")
      val iddsa = new File(options.sshUserDir, "id_dsa")
      if (idrsa.exists) jsch.addIdentity(idrsa.getAbsolutePath)
      if (iddsa.exists) jsch.addIdentity(iddsa.getAbsolutePath)
      jschsession = jsch.getSession(options.username, options.host, options.port)
      jschsession setUserInfo SSHUserInfo(options.password, options.passphrase)
      jschsession.connect(options.timeout.toInt)
    }
    jschsession
  }

  def execOnceAndTrim(scmd: SSHCommand) = execOnce(scmd).trim()

  def execOnce(scmd: SSHCommand) = {
    val sb = new StringBuilder()
    def recvStandardOutput(content: Option[String]) {
      for (c <- content) {
        sb.append(c)
        sb.append("\n")
      }
    }
    val runner = new SSHExec(scmd.cmd, recvStandardOutput, _ => None)
    runner.waitForEnd
    sb.toString()
  }
}

class SSHExec(cmd: String, out: Option[String] => Any, err: Option[String] => Any)(implicit ssh: SSH) {

  val (channel, stdout, stderr, stdin) = {
    val ch = ssh.jschsession.openChannel("exec").asInstanceOf[ChannelExec]
    ch.setCommand(cmd.getBytes())
    val stdout = ch.getInputStream()
    val stderr = ch.getErrStream()
    val stdin = ch.getOutputStream()
    ch.connect(ssh.options.timeout.toInt)
    (ch, stdout, stderr, stdin)
  }
  val stdoutThread = InputStreamThread(channel, stdout, out)
  val stderrThread = InputStreamThread(channel, stderr, err)

  def giveInputLine(line: String) {
    stdin.write(line.getBytes())
    stdin.write("\n".getBytes())
    stdin.flush()
  }

  def waitForEnd {
    stdoutThread.join()
    stderrThread.join()
  }

  class InputStreamThread(channel: ChannelExec, input: InputStream, output: Option[String] => Any) extends Thread {
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
  object InputStreamThread {
    def apply(channel: ChannelExec, input: InputStream, output: Option[String] => Any) = {
      val newthread = new InputStreamThread(channel, input, output)
      newthread.start()
      newthread
    }
  }

  def close() = channel.disconnect
}

class SSHFtp(implicit ssh: SSH) {
  private var jschftpchannel: ChannelSftp = null

  def channel = {
    if (jschftpchannel == null) {
      //jschftpchannel.connect(link.connectTimeout)
      var connected = false
      var retryCount = ssh.options.retryCount
      while (retryCount > 0 && !connected) {
        try {
          jschftpchannel = ssh.jschsession.openChannel("sftp").asInstanceOf[ChannelSftp]
          jschftpchannel.connect(ssh.options.timeout.toInt)
          connected = true
        } catch {
          case e =>
            try { jschftpchannel.disconnect } catch { case _ => } finally { jschftpchannel = null }
            retryCount -= 1
            if (retryCount > 0) Thread.sleep(ssh.options.retryDelay)
            else {
              println("SSH CONNECT Maximum retry count reached, couldn't connect to remote system " + ssh.options.host)
              e.printStackTrace
            }
        }
      }
    }
    jschftpchannel
  }

  def close() = {
    if (jschftpchannel != null) {
      jschftpchannel.quit
      jschftpchannel.disconnect
      jschftpchannel = null
    }
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

  val defaultPrompt = """-PRMT-: """
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

    val fromServer = new ConsumerOutputStream()
    ch.setOutputStream(fromServer)

    ch.connect(ssh.options.timeout.toInt)

    (ch, toServer, fromServer)
  }

  def batch[I <: Iterable[String]](commands: I)(implicit bf: CanBuildFrom[I, String, I]): I = {
    var builder = bf.apply()
    try {
      for (cmd <- commands) builder += (execute(cmd))
    }
    builder.result
  }

  def close() = {
    channel.disconnect()
  }

  def executeAndContinue(cmd: String, cont: String => Unit): Unit = {
    cont(execute(cmd))
  }

  def executeAndTrim(cmd: String): String = execute(cmd).trim()

  def executeAndTrimSplit(cmd: String): Array[String] = execute(cmd).trim().split("\r?\n")

  def execute(cmd: String, timeout: Long = ssh.options.timeout): String = {
    sendCommand(cmd)
    val result = fromServer.getResponse()
    result
  }

  private var doInit = true
  private def sendCommand(cmd: String): Unit = {
    if (doInit) {
      if (ssh.options.prompt.isEmpty) {
        // if no prompt is given we assume that a standard sh/bash/ksk shell is used
        toServer.sendCommand("unset LS_COLORS")
        toServer.sendCommand("unset EDITOR")
        toServer.sendCommand("unset PAGER")
        toServer.sendCommand("COLUMNS=500")
        toServer.sendCommand("PS1='%s'".format(defaultPrompt))
        toServer.sendCommand("echo 'started'")
        //toServer.sendCommand("set +o emacs")  // => Makes everything not working anymore, JSCH problem ?
        //toServer.sendCommand("set +o vi") // => Makes everything not working anymore, JSCH problem ?
        Thread.sleep(1000) // TODO : Bad but Mandatory to get some response from JSCH => Find a better way
        fromServer.getResponse()
        while (fromServer.hasResponse()) fromServer.getResponse()
      } else {
        Thread.sleep(1000) // TODO : Bad but Mandatory to get some response from JSCH => Find a better way
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
  }

  class ConsumerOutputStream extends OutputStream {

    val resultsQueue = new ArrayBlockingQueue[String](100)

    def now = System.currentTimeMillis()

    def hasResponse() = resultsQueue.size > 0

    def getResponse(timeout: Long = ssh.options.timeout) = {
      val started = now
      resultsQueue.take()
    }

    val consumerAppender = new StringBuilder(8192)
    var searchForPromptIndex = 0
    val promptSize = prompt.size
    var lastPromptChar = prompt.last

    def write(b: Int) {
      if (b != 13) { //CR removed... CR is always added by JSCH !!!!
        consumerAppender.append(b.toChar) // TODO - Add charset support
        if (consumerAppender.endsWith(prompt)) {
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
