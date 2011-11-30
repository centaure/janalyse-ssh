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

trait OutputMessage {
  val line:String
}
case class StandardOutputMessage(line:String) extends OutputMessage
case class StandardErrorMessage(line:String) extends OutputMessage


case class SSHOptions(
  username: String,
  password: String = "",
  passphrase: String = "",
  host: String = "localhost",
  port: Int = 22,
  connectTimeout: Int = 30000,
  retryCount:Int = 5,
  retryDelay:Int = 2000
)

object SSH extends SSHAutoClose {
  def ssh[T](
    username: String,
    password: String = "",
    passphrase: String = "",
    host: String = "localhost",
    port: Int = 22,
    connectTimeout: Int = 30000)(withssh: (SSH) => T) = usingSSH(new SSH(SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, connectTimeout = connectTimeout))) {
    withssh(_)
  }
  implicit def toCommand(cmd: String) = new SSHCommand(cmd)
  implicit def toBatchList(cmdList: List[String]) = new SSHBatch(cmdList)
  implicit def toRemoteFile(filename: String) = new SSHRemoteFile(filename)
}

class SSH(val options: SSHOptions) extends SSHAutoClose {
  private val ssh = this
  private val jsch = new JSch
  var jschsession: Session = initSession

  def apply[T](proc: (SSH) => T) = proc(this)

  def shell[T](proc: (SSHShell) => T) = usingSSH(new SSHShell) { proc(_) }

  def ftp[T](proc: (SSHFtp) => T) = usingSSH(new SSHFtp) { proc(_) }
  
  def run(cmd:String, clientActor:Actor) = usingSSH(new SSHExec) {_.run(cmd, clientActor)}

  def execute(cmd: SSHCommand) = shell { _ execute cmd.cmd }

  def execute(cmds: List[String]) = shell { _ batch cmds }

  def close() {
    if (jschsession != null) {
      jschsession.disconnect
      jschsession = null
    }
  }

  private def initSession = {
    if (jschsession == null || !jschsession.isConnected) {
      close
      val home = scala.util.Properties.userHome
      val idrsa = new File(home, ".ssh/id_rsa")
      val iddsa = new File(home, ".ssh/id_dsa")
      if (idrsa.exists) jsch.addIdentity(idrsa.getAbsolutePath)
      if (iddsa.exists) jsch.addIdentity(iddsa.getAbsolutePath)
      jschsession = jsch.getSession(options.username, options.host, options.port)
      jschsession setUserInfo SSHUserInfo(options.password, options.passphrase)
      jschsession.connect(options.connectTimeout)
    }
    jschsession
  }


  
  
  class SSHExec() {
    private var jschexecchannel  : ChannelExec = _
//    private var jschStdoutStream : PipedInputStream = _
//    private var jschStderrStream : PipedInputStream = _
    private var jschStdoutStream : InputStream = _
    private var jschStderrStream : InputStream = _
    
    def getChannel(cmd:String) = {
      if (jschexecchannel == null || jschexecchannel.isClosed() || jschexecchannel.isEOF()) {
        if (jschexecchannel!=null) close()
        var connected = false
        var retryCount = ssh.options.retryCount
        while (retryCount > 0 && !connected) {
          try {
            jschexecchannel = ssh.jschsession.openChannel("exec").asInstanceOf[ChannelExec]
            jschexecchannel.setCommand(cmd.getBytes())
            
            jschStdoutStream = jschexecchannel.getInputStream()
            jschStderrStream = jschexecchannel.getErrStream()
//            jschStdoutStream  = new PipedInputStream(32*1024)
//            jschStderrStream  = new PipedInputStream(32*1024)
//            jschexecchannel.setErrStream(new PipedOutputStream(jschStderrStream), true)
//            jschexecchannel.setOutputStream(new PipedOutputStream(jschStdoutStream), true)
            
            jschexecchannel.connect(ssh.options.connectTimeout)
            connected = true
          } catch {
            case e =>
              try { jschexecchannel.disconnect} catch { case _ => } finally { jschexecchannel = null }
              retryCount -= 1
              if (retryCount > 0) Thread.sleep(ssh.options.retryDelay)
              else {
                println("SSH CONNECT Maximum retry count reached, couldn't connect to remote system " + ssh.options.host)
                e.printStackTrace
              }
          }
        }
      }
      jschexecchannel
    }
    
    def run(cmd:String, clientActor:Actor) = {
      val channel = getChannel(cmd)
      val stdout  = InputStreamThread(channel, jschStdoutStream, clientActor, StandardOutputMessage(_))
      val stderr  = InputStreamThread(channel, jschStderrStream, clientActor, StandardErrorMessage(_))
      val stdin   = OutputStreamActor(channel, channel.getOutputStream())
      stdin
    }
    
    
    class InputStreamThread(channel:ChannelExec, input:InputStream, clientActor:Actor, msgBuilder:(String)=>OutputMessage) extends Thread {
      override def run() {
        val bufsize = 16*1024
    	val charset = Charset.forName("ISO-8859-15")
    	val binput  = new BufferedInputStream(input)
        val bytes   = Array.ofDim[Byte](bufsize)
    	val buffer  = ByteBuffer.allocate(bufsize)
    	do {
    	  val available = binput.available()
    	  val howmany   = binput.read(bytes, 0, if (available < bufsize) available else bufsize)
    	  if (howmany>0) {
    	    buffer.put(bytes, 0, howmany)
    	    buffer.flip()
    	    val cbOut = charset.decode(buffer)
    	    println("==>"+cbOut.toString())
    	  }
    	  println("avail=%d read=%d eof=%s closed=%s".format(available, howmany, channel.isEOF().toString(), channel.isClosed().toString))
    	  Thread.sleep(1000)
    	} while(/*!channel.isEOF() && !channel.isClosed()*/ true)
    	println("CLOSED")
      }
    }
    object InputStreamThread {
      def apply(channel:ChannelExec, input:InputStream, clientActor:Actor, msgBuilder:(String)=>OutputMessage) = { 
        val newthread = new InputStreamThread(channel, input, clientActor, msgBuilder)
        newthread.start()
        newthread
      }
    }
    
    class OutputStreamActor(channel:ChannelExec, output:OutputStream) extends Actor {
      val pout = new java.io.PrintStream(output)
      def act() {
        loop {
          receive {
          	case str:String =>  pout.println(str)
          }
        }
      }
    }
    
    object OutputStreamActor {
      def apply(channel:ChannelExec, output:OutputStream) = {
        val newactor = new OutputStreamActor(channel, output)
        newactor.start()
        newactor
      }
    }
    
    
    def close() = {
      if (jschexecchannel != null) {
        jschexecchannel.disconnect
        jschexecchannel = null
      }
    }
  }

  
  class SSHFtp {
    private var jschftpchannel: ChannelSftp = null

    def channel = {
      if (jschftpchannel == null) {
        //jschftpchannel.connect(link.connectTimeout)
        var connected = false
        var retryCount = ssh.options.retryCount
        while (retryCount > 0 && !connected) {
          try {
            jschftpchannel = ssh.jschsession.openChannel("sftp").asInstanceOf[ChannelSftp]
            jschftpchannel.connect(ssh.options.connectTimeout)
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

    def getRaw(filename: String): Option[Array[Byte]] = {
      try {
        Some(SSHTools.inputStream2ByteArray(channel.get(filename)))
      } catch {
        case e: SftpException if (e.id == 2) => None // File doesn't exist
        case e: IOException => None
      }
    }

  }


  class SSHShell(timeout: Long = 100000L) {

    def batch[I <: Iterable[String]](commands: I)(implicit bf: CanBuildFrom[I, String, I]): I = {
      var builder = bf.apply()
      try {
        for (cmd <- commands) builder += (execute(cmd))
      }
      builder.result
    }

    val prompt = """-PRMT-: """
    val inout = new PipedOutputStream
    val inin = new PipedInputStream(inout)
    val bout = new MyOut(self)

    val channel = {
      var channel: ChannelShell = null
      var connected: Boolean = false
      var retryCount = ssh.options.retryCount
      while (retryCount > 0 && !connected) {
        try {
          channel = ssh.jschsession.openChannel("shell").asInstanceOf[ChannelShell]
          channel.setPtyType("dumb")
          //channel.setPtyType("")
          channel.setXForwarding(false)

          channel.setInputStream(inin)
          channel.setOutputStream(bout)
          channel.connect(ssh.options.connectTimeout)
          connected = true
        } catch {
          case e =>
            try { channel.disconnect } catch { case _ => } finally { channel = null }
            retryCount -= 1
            if (retryCount > 0) Thread.sleep(ssh.options.retryDelay)
            else {
              println("SSH CONNECT Maximum retry count reached, couldn't connect to remote system " + ssh.options.host)
              e.printStackTrace
            }
        }
      }

      sendCommand("""unset LS_COLORS """)
      sendCommand("""export PS1='%s'""".format(prompt))
      channel
    }

    def close() = {
      inout.close
      inin.close
      channel.disconnect
    }

    def executeAndContinue(cmd: String, cont: String => Unit): Unit = {
      cont(execute(cmd))
    }

    def execute(cmd: String, timeout: Long = timeout): String = {
      sendCommand(cmd)
      getResponse(timeout)
    }

    private def sendCommand(cmd: String): Unit = {
      inout.write(cmd.getBytes)
      inout.write("\n".getBytes)
      inout.flush()
    }

    private def getResponse(timeout: Long = timeout, breaktry: Int = 5): String = {
      receiveWithin(timeout) {
        case TIMEOUT => // Sending Break command ctrl-c
          inout.write(28.toChar.toString.getBytes)
          if (breaktry > 0) getResponse(timeout, breaktry - 1) else "FAILED TO BREAK"
        case v @ _ => v.toString
      }
    }

    // ATTENTION Appelé par un thread tiers (appartenant à jsch)
    class MyOut(caller: Actor) extends OutputStream {
      var boot = true // Amorçage réalisé une fois le prompt effectivement modifié, on ignore tout ce qui a pu etre réalisé précédement
      var waitFirstPrompt = true // On attend la première apparition du prompt, on ignore ce qui a pu se passer précédemment
      var lines = List[String]()
      var line = ""
      def write(b: Int) = b match {
        case 13 =>
        case 10 =>
          if (boot && line.startsWith(prompt)) {
            boot = false
            lines = List(lines.last)
          } else
            lines = lines :+ line
          line = ""
        case _ =>
          line += b.toChar
          if (!boot && (line endsWith prompt)) {
            //if (!waitFirstPrompt) {
            lines = lines :+ line.dropRight(prompt.size)
            caller ! (lines.tail mkString "\n")
            //} else waitFirstPrompt=false
            lines = List[String]()
            line = ""
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
case class SSHUserInfo(password: String, passphrase: String = "") extends UserInfo with UIKeyboardInteractive {
  override def getPassphrase() = passphrase
  override def getPassword() = password
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
