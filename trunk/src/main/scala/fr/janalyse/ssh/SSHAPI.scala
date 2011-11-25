package fr.janalyse.ssh

import com.jcraft.jsch._
import scala.actors._
import scala.actors.Actor._
import scala.io.BufferedSource
import java.util.logging._
import java.io._
import scala.collection.generic.CanBuildFrom

// =============================================================================
/*
trait AutoClose {
  // Automatic resource liberation
  def using[T <: { def close}, R] (resource: T) (block: T => R) = {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close
    }
  }
}

case class Credentials(username:String, password:String)


abstract class Connector {
  val name:String
  val ip:String
  val port:Int
  val credentials:Option[Credentials]
  val props:Map[String,String]
}

case class SSHConnector(name:String, ip:String, port:Int, credentials:Option[Credentials], props:Map[String,String]=Map.empty) extends Connector

object SSHConnector {
  implicit def string2SSHConnector(spec:String):SSHConnector = SSHConnector(spec)
  
  def apply(spec:String) = {
    spec.trim.split(":") match {
      case Array(name,username,password)              => new SSHConnector(name=name, ip="localhost", port=22, credentials=Some(Credentials(username, password)))
      case Array(name,host,username,password)         => new SSHConnector(name=name, ip=host, port=22, credentials=Some(Credentials(username, password)))
      case Array(name,host,username,password,port,_*) => new SSHConnector(name=name, ip=host, port=port.toInt, credentials=Some(Credentials(username, password)))
    }
  }
}

*/

// =============================================================================

/*
class SSHListExecutor(commands:List[String])(implicit session:SSH) extends AutoClose {
  def execute = using(session.shell) { sh => commands map { sh execute _ }}
}

class SSHStringExecutor(command:String)(implicit session:SSH) extends AutoClose {
  def execute = using(session.shell) { _ execute command }
}

class FTPSender(data:String)(implicit session:SSH) extends AutoClose {
  def put(filename:String) {using(session.ftp) {_.put(data,filename)}}
}

class FTPFileSender(localFilename:String)(implicit session:SSH) extends AutoClose {
  def send(remoteFilename:String) {using(session.ftp) {_.send(localFilename,remoteFilename)}}
}

class FTPReceiver(remoteFilename:String)(implicit session:SSH) extends AutoClose {
  def get = {using(session.ftp) {_.get(remoteFilename)}}
}

class FTPFileReceiver(remoteFilename:String)(implicit session:SSH) extends AutoClose {
  def receive(localFilename:String) = {using(session.ftp) {_.receive(remoteFilename, localFilename)}}
  def receive(localFile:File) = {using(session.ftp) {_.receive(remoteFilename, localFile)}}
}
*/


/*
object SSH {
  
  def apply(c :SSHConnector) = new SSH(username=c.credentials.get.username, password=c.credentials.get.password, host=c.ip, port=c.port)

  // Implicits conversions
  implicit def connector2session(connector:SSHConnector) = SSH(connector)
  //implicit def server2session(server:Server) = SSH(server)
  implicit def list2Shell(commands:List[String])(implicit session:SSH) = new SSHListExecutor(commands)
  implicit def string2Shell(command:String)(implicit session:SSH) = new SSHStringExecutor(command)
  implicit def string2FTPSender(data:String)(implicit session:SSH) = new FTPSender(data)
  implicit def string2FTPFileSender(localFilename:String)(implicit session:SSH) = new FTPFileSender(localFilename)
  implicit def string2FTPReceiver(remoteFilename:String)(implicit session:SSH) = new FTPReceiver(remoteFilename)
  implicit def string2FTPFileReceiver(remoteFilename:String)(implicit session:SSH) = new FTPFileReceiver(remoteFilename)
  
}*/


trait AutoClose {
  // Automatic resource liberation
  def using[T <: { def close()}, R] (resource: T) (block: T => R) = {
    try     block(resource)
    finally resource.close
  }
}


class SSHCommand(val cmd:String) {
  def !(implicit ssh:SSH) = {
    ssh.shell { _ execute cmd}
  }
}

class SSHBatch(val cmdList:List[String]) {
  def !(implicit ssh:SSH) = {
    ssh.shell { _ batch cmdList}
  }  
}


class SSHRemoteFile(val filename:String) {
  def get(implicit ssh:SSH) = {
    ssh.ftp {_ get filename}
  }
}


case class SSHOptions(
    username:String,
    password:String="",
    passphrase:String="",
    host:String="localhost",
    port:Int=22,
    connectTimeout:Int=30000)


    
object SSH extends AutoClose  {
  def ssh[T](
    username:String,
    password:String="",
    passphrase:String="",
    host:String="localhost",
    port:Int=22,
    connectTimeout:Int=30000)
    (withssh:(SSH)=>T) = using(new SSH(SSHOptions(username=username,password=password, passphrase=passphrase, host=host, port=port, connectTimeout=connectTimeout))) {
      withssh(_)
    }
  implicit def toCommand(cmd:String) = new SSHCommand(cmd)
  implicit def toRemoteFile(filename:String) = new SSHRemoteFile(filename)
  implicit def toBatchList(cmdList:List[String]) = new SSHBatch(cmdList)
}




    
    

class SSH(val options:SSHOptions) extends AutoClose {
  private val jsch = new JSch
  private var jschsession:Session = session

  def apply[T](proc:(SSH)=>T) =  proc(this)
  
  def shell[T](proc:(SSHShell)=>T) = using(new SSHShell(this)) { proc(_) }

  def ftp[T](proc:(SSHFtp)=>T) = using(new SSHFtp(this)) { proc(_) }
  
  def execute(cmd:SSHCommand):String = shell { _ execute cmd.cmd }
  
  def close() { if (jschsession!=null) {
      jschsession.disconnect
      jschsession=null
    }
  }
  
  
  def session = {
    if (jschsession==null || !jschsession.isConnected) {
      close
      val home = scala.util.Properties.userHome
      val idrsa = new File(home,".ssh/id_rsa")
      val iddsa = new File(home,".ssh/id_dsa")
      if (idrsa.exists) jsch.addIdentity(idrsa.getAbsolutePath)
      if (iddsa.exists) jsch.addIdentity(iddsa.getAbsolutePath)
      jschsession = jsch.getSession(options.username, options.host, options.port)
      jschsession setUserInfo SSHUserInfo(options.password, options.passphrase)
      jschsession.connect(options.connectTimeout)
    }
    jschsession
  }
    
}

// =============================================================================

class SSHFtp(link:SSH) {
  private var jschftpchannel:ChannelSftp=null

  var retryCount=5
  var retryDelay=2000

  def channel = {
    if (jschftpchannel==null) {
      //jschftpchannel.connect(link.connectTimeout)
      var connected=false
      while(retryCount>0 && !connected) {
        try {
          jschftpchannel=link.session.openChannel("sftp").asInstanceOf[ChannelSftp]
          jschftpchannel.connect(link.options.connectTimeout)
          connected=true
        } catch {
          case e =>
            try { jschftpchannel.disconnect} catch {case _=> } finally { jschftpchannel=null }
            retryCount-=1
            if (retryCount>0) Thread.sleep(retryDelay)
            else {
              println("SSH CONNECT Maximum retry count reached, couldn't connect to remote system "+link.options.host)
              e.printStackTrace
            }
        }
      }
    }
    jschftpchannel
  }

  def close() = {
    if (jschftpchannel!=null) {
      jschftpchannel.quit
      jschftpchannel.disconnect
      jschftpchannel=null
    }
  }

  def get(filename:String):Option[String] = {
    try {
      Some(new BufferedSource(channel.get(filename)).mkString)
    } catch {
      case e:SftpException if (e.id==2) => None  // File doesn't exist
      case e:IOException => None
    }
  }
  def put(data:String, remoteFilename:String) {
    channel.put(new ByteArrayInputStream(data.getBytes), remoteFilename)
  }

  def receive(remoteFilename:String, localFile:File) = {
    try {
      channel.get(remoteFilename, new FileOutputStream(localFile))
    } catch {
      case e:SftpException if (e.id==2) => None  // File doesn't exist
      case e:IOException => None
    }
  }
  def receive(remoteFilename:String, localFilename:String) = {
    try {
      channel.get(remoteFilename, new FileOutputStream(localFilename))
    } catch {
      case e:SftpException if (e.id==2) => None  // File doesn't exist
      case e:IOException => None
    }
  }
  def send(localFilename:String, remoteFilename:String) = {
      channel.put(new FileInputStream(localFilename), remoteFilename)
  }

  def getRaw(filename:String):Option[Array[Byte]] = {
    try {
      Some(SSHTools.inputStream2ByteArray(channel.get(filename)))
    } catch {
      case e:SftpException if (e.id==2) => None  // File doesn't exist
      case e:IOException => None
    }
  }

}


// =============================================================================

class SSHShell(link:SSH, val timeout:Long=100000L) {
  
  def !(command:String) = execute(command)
  
  def batch[ I<:Iterable[String]](commands:I) (implicit bf: CanBuildFrom[I, String, I]) : I = {
    var builder = bf.apply()
    try {
      for (cmd<-commands) builder += (execute(cmd))
    }    
    builder.result
  }

  
  
  val prompt="""-PRMT-: """
  val inout = new PipedOutputStream
  val inin = new PipedInputStream(inout)
  val bout = new MyOut(self)

  var retryCount=10
  var retryDelay=10000

  val channel = {
    var channel:ChannelShell=null
    var connected:Boolean = false
    while(retryCount>0 && !connected) {
      try {
        channel = link.session.openChannel("shell").asInstanceOf[ChannelShell]
        channel.setPtyType("dumb")
        //channel.setPtyType("")
        channel.setXForwarding(false)

        channel.setInputStream(inin)
        channel.setOutputStream(bout)
        channel.connect(link.options.connectTimeout)
        connected=true
      } catch {
        case e =>
          try { channel.disconnect } catch {case _=> } finally { channel=null }
          retryCount-=1
          if (retryCount>0) Thread.sleep(retryDelay)
          else {
            println("SSH CONNECT Maximum retry count reached, couldn't connect to remote system "+link.options.host)
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

    def executeAndContinue(cmd:String, cont:String=>Unit):Unit = {
        cont(execute(cmd))
    }

    def execute(cmd:String, timeout:Long = timeout) : String = {
       sendCommand(cmd)
       getResponse(timeout)
    }

    private def sendCommand(cmd:String):Unit = {
       inout.write(cmd.getBytes)
       inout.write("\n".getBytes)
       inout.flush()
    }

    private def getResponse( timeout:Long=timeout, breaktry:Int=5) : String = {
       receiveWithin(timeout) {
         case TIMEOUT => // Sending Break command ctrl-c
            inout.write(28.toChar.toString.getBytes)
            if (breaktry>0) getResponse(timeout, breaktry-1) else "FAILED TO BREAK"
         case v@_ => v.toString
       }
    }

    // ATTENTION Appelé par un thread tiers (appartenant à jsch)
    class MyOut(caller:Actor) extends OutputStream {
      var boot=true // Amorçage réalisé une fois le prompt effectivement modifié, on ignore tout ce qui a pu etre réalisé précédement
      var waitFirstPrompt=true // On attend la première apparition du prompt, on ignore ce qui a pu se passer précédemment
      var lines = List[String]()
      var line=""
      def write(b: Int) = b match {
        case 13 =>
        case 10 =>
          if (boot && line.startsWith(prompt)) {
            boot=false
            lines=List(lines.last)
          } else
            lines=lines:+line
          line=""
        case _ =>
          line+=b.toChar
          if (!boot && (line endsWith prompt)) {
            //if (!waitFirstPrompt) {
              lines=lines:+line.dropRight(prompt.size)
              caller ! (lines.tail mkString "\n")
            //} else waitFirstPrompt=false
            lines=List[String]()
            line=""
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
case class SSHUserInfo(password:String, passphrase:String="") extends UserInfo with UIKeyboardInteractive  {
  override def getPassphrase() = passphrase
  override def getPassword() = password
  override def promptPassword(message:String) = true
  override def promptPassphrase(message:String ) = false
  override def promptYesNo(message:String ) = true
  override def showMessage(message:String) = {}
  override def promptKeyboardInteractive(destination:String, name:String, instruction:String, prompt:Array[String], echo:Array[Boolean]) : Array[String] =  Array(getPassword())
}

// =============================================================================

object SSHTools {
  def getFile(filename:String):String = {
    new BufferedSource(new FileInputStream(filename)).mkString
  }
  def getRawFile(filename:String):Array[Byte] = {
    inputStream2ByteArray(new FileInputStream(filename))
  }
  def inputStream2ByteArray(input:InputStream):Array[Byte] = {
    val fos    = new ByteArrayOutputStream(65535)
    val bfos   = new BufferedOutputStream(fos, 16384)
    val bis    = new BufferedInputStream(input)
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
  def basename(name:String, ext:String) = if (name contains ext) name.substring(0,name.indexOf(ext)) else name

}
