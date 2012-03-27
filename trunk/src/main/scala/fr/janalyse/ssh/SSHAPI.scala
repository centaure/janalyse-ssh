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
import scala.util.{Properties=>SP}
import java.io.File.{separator=>FS, pathSeparator=>PS}
import scala.collection.mutable.SynchronizedQueue

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
  prompt: Option[String]=None,
  timeout: Long = 30000,
  retryCount: Int = 5,
  retryDelay: Int = 2000,
  sshUserDir: String = SP.userHome+FS+".ssh",
  charset:String = "ISO-8859-15"
  )

object SSH extends SSHAutoClose {
  
  def connect[T](
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000)(withssh: (SSH) => T):T = usingSSH(new SSH(SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout))) {
    withssh(_)
  }
  def connect[T](options:SSHOptions)(withssh: (SSH) => T) = usingSSH(new SSH(options)) {
    withssh(_)
  }
  def connect[T](someOptions:Option[SSHOptions])(withssh: (SSH) => Option[T]) = someOptions map { options => usingSSH(new SSH(options)) {
	  withssh(_)
  	}
  }
  
  
  def shell[T](
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000)(withsh: (SSHShell) => T):T = shell[T](SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout)) (withsh)
  
  def shell[T](options:SSHOptions)(withsh: (SSHShell) => T):T = usingSSH(new SSH(options)) {ssh =>
    ssh.shell {sh => withsh(sh)}
  }
  def shell[T](someOptions:Option[SSHOptions])(withsh: (SSHShell) => T):Option[T] =  someOptions map {shell[T](_)(withsh)}
  
  def ftp[T](
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000)(withftp: (SSHFtp) => T):T = ftp[T](SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout)) (withftp)
  
  def ftp[T](options:SSHOptions)(withftp: (SSHFtp) => T):T = usingSSH(new SSH(options)) {ssh =>
    ssh.ftp {ftp => withftp(ftp)}
  }
  def ftp[T](someOptions:Option[SSHOptions])(withftp: (SSHFtp) => T):Option[T] = someOptions map { ftp[T](_)(withftp) }

  
  
  def shellAndFtp[T](
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000)(withshftp: (SSHShell,SSHFtp) => T):T = shellAndFtp[T](SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout)) (withshftp)
  
  def shellAndFtp[T](options:SSHOptions)(withshftp: (SSHShell,SSHFtp) => T):T = usingSSH(new SSH(options)) {ssh =>
    ssh.shell {sh => ssh.ftp { ftp =>withshftp(sh,ftp)}}
  }
  def shellAndFtp[T](someOptions:Option[SSHOptions])(withshftp: (SSHShell,SSHFtp) => T):Option[T] = someOptions map {shellAndFtp[T](_)(withshftp)}
 
  
  def apply(
    username: String = util.Properties.userName,
    password: Option[String] = None,
    passphrase: Option[String] = None,
    host: String = "localhost",
    port: Int = 22,
    timeout: Int = 30000) = new SSH(SSHOptions(username = username, password = password, passphrase = passphrase, host = host, port = port, timeout = timeout))

  def apply(options:SSHOptions) = new SSH(options)
  def apply(someOptions:Option[SSHOptions]):Option[SSH] = someOptions map {new SSH(_)}
  
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

  def noerr(data:Option[String]) {}
  
  def run(cmd: String, out: Option[String]=> Any, err: Option[String]=> Any = noerr) = new SSHExec(cmd, out, err)

  def execute(cmd: SSHCommand) = shell { _ execute cmd.cmd }
  
  def execute(cmds: SSHBatch) = shell { _ batch cmds.cmdList }
  
  def executeAndTrim(cmd: SSHCommand) = execute(cmd).trim()

  def executeAndTrimSplit(cmd: SSHCommand) = execute(cmd).trim().split("\r?\n")

  def executeAndTrim(cmds: SSHBatch) = execute(cmds.cmdList) map {_.trim}

  def executeAndTrimSplit(cmds: SSHBatch) = execute(cmds.cmdList) map {_.trim.split("\r?\n")}

  def get(remoteFilename:String) = ssh.ftp { _ get remoteFilename }
  
  def getBytes(remoteFilename:String) = ssh.ftp { _ getBytes remoteFilename }

  def put(data: String, remoteFilename:String) = ssh.ftp { _ put (data, remoteFilename) }
  
  def receive(remoteFilename:String, toLocalFilename: String) = ssh.ftp { _.receive(remoteFilename, toLocalFilename) }
  
  def send(fromLocalFilename: String, remoteFilename:String) = ssh.ftp { _.send(fromLocalFilename, remoteFilename)
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
  
}


  case class EndMessage


  class SSHExec(cmd: String, out: Option[String]=> Any, err: Option[String]=> Any) (implicit ssh:SSH) extends DaemonActor {
    private var jschexecchannel: ChannelExec  = _
    private var jschStdoutStream: InputStream = _
    private var jschStderrStream: InputStream = _
    private var jschStdinStream: OutputStream = _

    def getChannel(cmd: String) = {
      if (jschexecchannel == null || jschexecchannel.isClosed() || jschexecchannel.isEOF()) {
        if (jschexecchannel != null) close()
        var connected = false
        var retryCount = ssh.options.retryCount
        while (retryCount > 0 && !connected) {
          try {
            jschexecchannel = ssh.jschsession.openChannel("exec").asInstanceOf[ChannelExec]
            jschexecchannel.setCommand(cmd.getBytes())

            jschStdoutStream = jschexecchannel.getInputStream()
            jschStderrStream = jschexecchannel.getErrStream()
            jschStdinStream = jschexecchannel.getOutputStream()
            jschexecchannel.connect(ssh.options.timeout.toInt)

            connected = true
          } catch {
            case e =>
              e.printStackTrace()
              try { jschexecchannel.disconnect } catch { case _ => } finally { jschexecchannel = null }
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

    start()

    def act() {
      val channel = getChannel(cmd)
      val stdout = InputStreamThread(channel, jschStdoutStream, out)
      val stderr = InputStreamThread(channel, jschStderrStream, err)
      loop {
        receive {
          case str: String => jschStdinStream.write(str.getBytes())
          case _:EndMessage => exit()
        }
      }
    }

    class InputStreamThread(channel: ChannelExec, input: InputStream, output: Option[String]=> Any) extends Thread {
      override def run() {
        val bufsize = 16 * 1024
        val charset = Charset.forName(ssh.options.charset)
        val binput = new BufferedInputStream(input)
        val bytes = Array.ofDim[Byte](bufsize)
        val buffer = ByteBuffer.allocate(bufsize)
        val appender = new StringBuilder()
        do {
          val available = binput.available()
          if (available ==0) {
            // TODO => looks like we can't remove that with traditionnal IO... missing selectors...
            // But it enables us to Interrupt this loop...
        	Thread.sleep(250) 
          } else {
	          val howmany = binput.read(bytes, 0, if (available < bufsize) available else bufsize)
	          if (howmany > 0) {
	            buffer.put(bytes, 0, howmany)
	            buffer.flip()
	            val cbOut = charset.decode(buffer)
	            buffer.compact()
	            appender.append(cbOut.toString())
	            var s=0
	            var e=0
	            do {
	              e = appender.indexOf("\n",s)
	              if (e>=0) {
	                output(Some(appender.substring(s,e)))
	                s = e + 1
	              }
	            } while(e != -1 )
	            appender.delete(0,s)
	          }
          }
        } while (!channel.isEOF() && !channel.isClosed())
        if (appender.size>0) output(Some(appender.toString()))
        output(None)
      }
    }
    object InputStreamThread {
      def apply(channel: ChannelExec, input: InputStream, output: Option[String]=> Any) = {
        val newthread = new InputStreamThread(channel, input, output)
        newthread.start()
        newthread
      }
    }

    def close() = {
      this ! EndMessage
      if (jschexecchannel != null) {
        jschexecchannel.disconnect
        jschexecchannel = null
      }
    }
  }

  class SSHFtp (implicit ssh:SSH) {
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

  
  
  
  
  
  
  
  class SSHShell (implicit ssh:SSH) {

    val defaultPrompt = """-PRMT-: """
    val prompt = ssh.options.prompt getOrElse defaultPrompt
        
    val (channel, toServer, fromServer) = {
      var ch: ChannelShell = ssh.jschsession.openChannel("shell").asInstanceOf[ChannelShell]
      ch.setPtyType("dumb")
      ch.setXForwarding(false)
      //ch.setInputStream(null)
      //ch.setOutputStream(null)
      
      val fromServer = new ConsumerOutputStream()
      ch.setOutputStream(fromServer)
      
      //val toServer = new ProducerInputStream()
      //ch.setInputStream(toServer)
      val toServer = new PipedOutputStream
      ch.setInputStream(new PipedInputStream(toServer))
      
      ch.connect(ssh.options.timeout.toInt)

        if (ssh.options.prompt.isEmpty) {
          println("**** INIT STARTED ****")
	      //toServer.sendCommand("""unset LS_COLORS """)
	      //toServer.sendCommand("""unset EDITOR""")
	      //toServer.sendCommand("""set +o emacs""")
	      //toServer.sendCommand("""set +o vi""")
	      //toServer.sendCommand("""PS1='%s'""".format(defaultPrompt))  // ok with ksh, bash, sh
	      toServer.write("unset LS_COLORS\n".getBytes()) ; toServer.flush()
	      toServer.write("unset EDITOR\n".getBytes()) ; toServer.flush()
	      toServer.write("set +o emacs\n".getBytes()) ; toServer.flush()
	      toServer.write("set +o vi\n".getBytes()) ; toServer.flush()
	      toServer.write("PS1='%s'\n".format(defaultPrompt).getBytes()) ; toServer.flush()
	      toServer.flush()
  	      //fromServer.getResponse()
  	      //fromServer.getResponse()
          println("**** INIT FINISHED ****")
        }

      
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

    def execute(cmd: String, timeout: Long = ssh.options.timeout): String = {
      sendCommand(cmd)
      fromServer.getResponse()
    }
    
    def executeAndTrim(cmd:String):String = execute(cmd).trim()

    def executeAndTrimSplit(cmd:String):Array[String] = execute(cmd).trim().split("\r?\n")

    private def sendCommand(cmd: String): Unit = {
      //toServer.sendCommand(cmd)
      toServer.write( (cmd+"\n").getBytes()) ; toServer.flush()
    }

    
    private def rawWrite(cmd:String):Unit = {
      toServer.write(cmd.getBytes)
      toServer.write(10)
      toServer.flush()
    }
    
    
    
    class ProducerInputStream() extends InputStream {
      
      val cmdQueue = new SynchronizedQueue[String]()
      
      def sendCommand(cmd:String) {
        cmdQueue.enqueue(cmd+"\n")
        synchronized {
          lastKnownBufferSize += cmd.size+1
        }
      }
      
      val buffer=new StringBuilder()
      var lastKnownBufferSize=0
      
      override def read():Int = {
        while(lastKnownBufferSize==0 && cmdQueue.size==0) Thread.sleep(100)
        if(cmdQueue.size > 0) buffer.append(cmdQueue.dequeue())
        synchronized {
          lastKnownBufferSize -= 1
        }
        val c = buffer.charAt(0)
        buffer.deleteCharAt(0)
        print(c)
        c.toInt
      }
      
      override def available():Int = {
        lastKnownBufferSize
      }
    }

    
    
    
    
    
    class ProducerThread(output: OutputStream) extends Thread {
      setDaemon(true)
      setName("SSHShellConsumerThread")
      
      val cmdQueue = new SynchronizedQueue[String]()
      
      def sendCommand(cmd:String) {
        cmdQueue.enqueue(cmd)
      }
      
      override def run() {
        while(true) {
          if (cmdQueue.isEmpty) Thread.sleep(100)
          else {
            val cmd = cmdQueue.dequeue()
            output.write(cmd.getBytes)
            output.write("\n".getBytes)
            output.flush()              
          }
        }
      }
    }
    
    
    class ConsumerOutputStream extends OutputStream {
      val resultsQueue = new SynchronizedQueue[String]()
      def now=System.currentTimeMillis()
      
      def getResponse(timeout:Long = ssh.options.timeout) = {
        val started=now
        while(resultsQueue.isEmpty && (now-started<timeout)) Thread.sleep(100) // TODO : BAD BAD BAD - Temporary hack
        resultsQueue.dequeue()
      }
      
      val consumerAppender = new StringBuilder()
      var searchForPromptIndex=0
      
      var toGarbageCount=1
      
      def write(b: Int) {
        consumerAppender.append(b.toChar)
        val promptIndex=consumerAppender.indexOf(prompt, searchForPromptIndex)
        if (promptIndex != -1) {
  	      val firstNlIndex=consumerAppender.indexOf("\n")
	      val result = consumerAppender.substring(firstNlIndex+1, promptIndex)
	        println("-----------------------")
	        println(result)
	      if (toGarbageCount>0) toGarbageCount-=1
	      else resultsQueue.enqueue(result)
          searchForPromptIndex=0
          consumerAppender.clear()
        } else  {
          searchForPromptIndex = consumerAppender.size - prompt.size
          if (searchForPromptIndex<0) searchForPromptIndex=0
        }
      }
    }
    
    
    
    class ConsumerThread(input: InputStream) extends Thread {
      setDaemon(true)
      setName("SSHShellConsumerThread")
      
      val resultsQueue = new SynchronizedQueue[String]()
      
      val consumerBufSize = 8 * 1024
      val consumerCharset = Charset.forName(ssh.options.charset)
      val consumerInput = input // No buffer 
      val consumerBytesArray = Array.ofDim[Byte](consumerBufSize)
      val consumerBuffer = ByteBuffer.allocate(consumerBufSize)
      val consumerAppender = new StringBuilder()
      def now=System.currentTimeMillis()
      
      def getResponse(timeout:Long = ssh.options.timeout) = {
        val started=now
         // TODO : BAD BAD BAD - Temporary hack
        while(resultsQueue.isEmpty && (now-started<timeout)) Thread.sleep(100)
        resultsQueue.dequeue()
      }
      
      override def run() {
        while(true) {
	        var promptFound=false
	        var searchForPromptIndex=0
	        var lastReadtime=now
	        do {
	          consumerInput.available() match {
	            case 0 =>
	              Thread.sleep(100)
	            case available =>
	              val max2read = if (available < consumerBufSize) available else consumerBufSize
		          val howmany = consumerInput.read(consumerBytesArray, 0, max2read)
		          if (howmany > 0) {
		            lastReadtime=now
		            consumerBuffer.put(consumerBytesArray, 0, howmany)
		            consumerBuffer.flip()
		            val cbOut = consumerCharset.decode(consumerBuffer)
		            consumerBuffer.compact()
		            consumerAppender.append(cbOut.toString())
		            val promptIndex=consumerAppender.indexOf(prompt, searchForPromptIndex)
		            if (promptIndex != -1) {
		              searchForPromptIndex=promptIndex
		              promptFound=true
		            } else searchForPromptIndex = 0 // consumerAppender.size - prompt.size
		          }
	          }
	        } while (!promptFound)
	        val firstNlIndex=consumerAppender.indexOf("\n")
	        val result = consumerAppender.substring(firstNlIndex+1, searchForPromptIndex)
	        println("-----------------------")
	        println(consumerAppender)
	        println("---------")
	        println(result)
	        resultsQueue.enqueue(result)
	        consumerAppender.clear()
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
case class SSHUserInfo(password: Option[String]=None, passphrase: Option[String] = None) extends UserInfo with UIKeyboardInteractive {
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
