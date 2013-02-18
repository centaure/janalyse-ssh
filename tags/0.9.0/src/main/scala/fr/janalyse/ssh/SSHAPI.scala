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




// ==========================================================================================



/**
  * SSHCommand class models ssh command
  * @author David Crosson
  */
class SSHCommand(val cmd: String) {
  def §§(implicit ssh: SSH) = ssh.shell { _ execute this }
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
class SSHBatch(val cmdList: List[String]) {
  def §§(implicit ssh: SSH) = ssh.shell { _ execute this }
}

/**
  * SSHBatch object implicit conversions container  
  * @author David Crosson
  */
object SSHBatch {
  implicit def stringListToBatchList(cmdList: List[String]) = new SSHBatch(cmdList)
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
 * TransfertOperations defines generic data transfer operations over SCP or SFTP
 */
trait TransfertOperations {
  /**
   * get remote file content as an optional String
   * @param remoteFilename file content to get
   * @return Some content or None if file was not found 
   */
  def get(remoteFilename: String):Option[String]

  /**
   * get remote file content as an optional bytes array
   * @param remoteFilename file content to get
   * @return Some content or None if file was not found 
   */
  def getBytes(remoteFilename: String):Option[Array[Byte]]

  /**
   * Copy a remote file to a local one
   * @param remoteFilename Source file name (on remote system)
   * @param localFile Destination file (local system)
   */
  def receive(remoteFilename: String, toLocalFile: File)
  
  /**
   * Copy a remote file to a local one
   * @param remoteFilename Source file name (on remote system)
   * @param localFilename Destination file name (local system)
   */
  def receive(remoteFilename: String, localFilename: String) {
    receive(remoteFilename, new File(localFilename))
  }
  
  /**
   * upload string content to a remote file, if file already exists, it is overwritten
   * @param data content to upload in the remote file
   * @param remoteDestination file content to get 
   */
  def put(data: String, remoteDestination: String)

  /**
   * upload bytes array content to a remote file, if file already exists, it is overwritten
   * @param data content to upload in the remote file
   * @param remoteDestination file content to get 
   */
  def putBytes(data: Array[Byte], remoteDestination: String)

  
  /**
   * Copy a local file to a remote one
   * @param fromLocalFilename Source file name (local system)
   * @param remoteDestination Destination file name (on remote system)
   */
  def send(fromLocalFilename: String, remoteDestination: String) {
    send(new File(fromLocalFilename), remoteDestination)
  }

  /**
   * Copy a local file to a remote one
   * @param fromLocalFile Source file (local system)
   * @param remoteDestination Destination file name (on remote system)
   */
  def send(fromLocalFile: File, remoteDestination: String)

}

// ==========================================================================================

/**
 * ShellOperations defines generic shell operations and common shell commands shortcuts
 */
trait ShellOperations {
  
  /**
   * Execute the current command and return the result as a string
   * @param cmd command to be executed
   * @return result string
   */
  def execute(cmd: SSHCommand): String


  /**
   * Execute the current batch (list of commands) and return the result as a string collection
   * @param cmds batch to be executed
   * @return result string collection
   */
  def execute(cmds: SSHBatch):List[String]

  
  /**
   * Execute the current command and pass the result to the given code
   * @param cmd command to be executed
   * @param cont continuation code
   */
  def executeAndContinue(cmd: SSHCommand, cont: String => Unit): Unit = cont(execute(cmd))

  /**
   * Execute the current command and return the result as a trimmed string
   * @param cmd command to be executed
   * @return result string
   */
  def executeAndTrim(cmd: SSHCommand): String = execute(cmd).trim()

  /**
   * Execute the current command and return the result as a trimmed splitted string
   * @param cmd command to be executed
   * @return result string
   */
  def executeAndTrimSplit(cmd: SSHCommand): Array[String] = execute(cmd).trim().split("\r?\n")
  
  /**
   * Execute the current batch (list of commands) and return the result as a string collection
   * @param cmds batch to be executed
   * @return result trimmed string collection
   */
  def executeAndTrim(cmds: SSHBatch) = execute(cmds.cmdList) map { _.trim }

  /**
   * Execute the current batch (list of commands) and return the result as a string collection
   * @param cmds batch to be executed
   * @return result trimmed splitted string collection
   */
  def executeAndTrimSplit(cmds: SSHBatch) = execute(cmds.cmdList) map { _.trim.split("\r?\n") }
  
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
  def uname:String = executeAndTrim("""uname 2>/dev/null""")
  
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
  def pwd():String = executeAndTrim("pwd")
  
  /**
   * Change current working directory to home directory
   * Of course this requires a persistent shell session to be really useful...
   */
  def cd() {execute("cd")}
  
  /**
   * Change current working directory to the specified directory
   * Of course this requires a persistent shell session to be really useful...
   * @param dirname directory name
   */
  def cd(dirname:String) {execute("""cd "%s" """.format(dirname))}
  
  /**
   * Get remote host name
   * @return host name
   */
  def hostname:String = executeAndTrim("""hostname""")

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


  /**
   * internal helper method
   */
  private def genoptcmd(cmd:String):Option[String] = {
    executeAndTrim("""%s 2>/dev/null""".format(cmd)) match {
      case "" => None
      case str => Some(str)
    }    
  }
  
  /**
   * Generic test usage
   */
  private def testFile(testopt:String, filename:String):Boolean = {
    val cmd = """test %s "%s" ; echo $?""".format(testopt, filename)
    executeAndTrim(cmd).toInt == 0
  }

  
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
  implicit def string2password(pass: String) = new SSHPassword(Some(pass))
  implicit def stringOpt2password(passopt: Option[String]) = new SSHPassword(passopt)
}

// ==========================================================================================


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
  charset: String = "ISO-8859-15",
  noneCipher:Boolean = true,
  compress:Option[Int]=None
)

  
  
// ==========================================================================================

  
/**
  * SSH object factories
  * @author David Crosson
  */
object SSH  {

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
    timeout: Int = 300000)(withssh: (SSH) => T): T = using(new SSH(SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))) {
    withssh(_)
  }
  /**
    * Executes the given code then closes the new ssh associated session. 
    * @param options ssh options
    * @param withssh code bloc to execute
    * @return "withssh" returns type
    */
  def once[T](options: SSHOptions)(withssh: (SSH) => T) = using(new SSH(options)) {
    withssh(_)
  }
  /**
    * Executes the given code then closes the new ssh associated session. 
    * @param someOptions Some ssh options or None, if None is given, nothing will be done 
    * @param withssh code bloc to execute
    * @return "withssh" returns type
    */
  def once[T](someOptions: Option[SSHOptions])(withssh: (SSH) => Option[T]) = someOptions map { options =>
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
    timeout: Int = 300000)(withsh: (SSHShell) => T): T = shell[T](SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))(withsh)

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
    timeout: Int = 300000)(withftp: (SSHFtp) => T): T = ftp[T](SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))(withftp)

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
    timeout: Int = 300000)(withshftp: (SSHShell, SSHFtp) => T): T = shellAndFtp[T](SSHOptions(host = host, username = username, password = password, passphrase = passphrase, port = port, timeout = timeout))(withshftp)

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
    val idrsa = new File(options.sshUserDir, "id_rsa")
    val iddsa = new File(options.sshUserDir, "id_dsa")
    if (idrsa.exists) jsch.addIdentity(idrsa.getAbsolutePath)
    if (iddsa.exists) jsch.addIdentity(iddsa.getAbsolutePath)
    val ses = jsch.getSession(options.username, options.host, options.port)
    ses.setTimeout(options.timeout.toInt)  // Timeout for the ssh connection (unplug cable to simulate) 
    ses setUserInfo SSHUserInfo(options.password.password, options.passphrase.password)
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

  def noerr(data: Option[String]) {}

  def run(cmd: String, out: Option[String] => Any, err: Option[String] => Any = noerr) = new SSHExec(cmd, out, err)
  
  override def execute(cmd: SSHCommand) =
    //shell { _ execute cmd.cmd }    // Using SSHShell channel  (lower performances)
    execOnce(cmd) // Using SSHExec channel (better performances)

    
  override def execute(cmds: SSHBatch) = shell { _ execute cmds.cmdList }


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

  
  override def get(remoteFilename: String):Option[String] = {
    //ssh.ftp { _ get remoteFilename }
    ssh.scp { _ get remoteFilename }
  }

  override def getBytes(remoteFilename: String):Option[Array[Byte]] = {
    //ssh.ftp { _ getBytes remoteFilename }
    ssh.scp { _ getBytes remoteFilename }
  }
  
  override def receive(remoteFilename: String, toLocalFile: File) {
    //ssh.ftp { _.receive(remoteFilename, toLocalFile) }
    ssh.scp { _.receive(remoteFilename, toLocalFile) }
  }  

  override def put(data: String, remoteDestination: String) {
    //ssh.ftp { _ put (data, remoteDestination) }        
    ssh.scp { _ put (data, remoteDestination) }
  }

  override def putBytes(data: Array[Byte], remoteDestination: String) {
    //ssh.ftp { _ putBytes (data, remoteDestination) }
    ssh.scp { _ putBytes (data, remoteDestination) }
  }

  override def send(fromLocalFile: File, remoteDestination: String) {
    //ssh.ftp {_.send(fromLocalFile, remoteDestination)}
    ssh.scp {_.send(fromLocalFile, remoteDestination)}
  }

  /**
   * Remote port -> local port
   * @param rport
   * @param lhost
   * @param lport
   */
  def remote2Local(rport:Int, lhost:String, lport:Int) {
    jschsession.setPortForwardingR(rport, lhost, lport)
  }
  
  /**
   * Local port -> Remote host with specified port
   * @param lport
   * @param rhost
   * @param rport
   */
  def local2Remote(lport:Int, rhost:String, rport:Int) {
    val aport = jschsession.setPortForwardingL(lport, rhost, rport);
  }
  
  
  def newShell = new SSHShell

  def newSftp = new SSHFtp

  def close() { jschsession.disconnect }

}



// ==========================================================================================


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

// ==========================================================================================


class SSHScp(implicit ssh: SSH) extends TransfertOperations {

 override def get(remoteFilename: String):Option[String] = {
    getBytes(remoteFilename).map(new String(_, ssh.options.charset))
  }

  override def getBytes(remoteFilename: String):Option[Array[Byte]] = {
    var filesBuffer = Map.empty[String, ByteArrayOutputStream]
    def filename2outputStream(filename:String) = {
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
    def filename2outputStream(filename:String) = new FileOutputStream(toLocalFile)
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
    val rfilename  = parts.last
    val rDirectory = if (parts.init.size==0) "." else parts.init.mkString("/")
    
    inputStream2remoteFile(linput, sz, rfilename, rDirectory)
  }

  override def send(fromLocalFile: File, remoteDestination: String) {
    val sz = fromLocalFile.length
    val linput = new FileInputStream(fromLocalFile)
    val parts = remoteDestination.split("/",-1)
    val rfilename  = if (parts.last.length==0) fromLocalFile.getName else parts.last
    val rDirectory = if (parts.init.size==0) "." else parts.init.mkString("/") 

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
      datasize:Long,
      remoteFilename:String,
      remoteDirectory:String) {
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
      var writtenBytes=0
      while (writtenBytes < datasize) {
         val c = bis.read()
         if (c>=0) {
           sout.write(c)
           writtenBytes+=1
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
      remoteFilenameMask:String,
      outputStreamBuilder:(String)=>OutputStream):Int = {
    val ch = ssh.jschsession.openChannel("exec").asInstanceOf[ChannelExec]
    try {
      ch.setCommand("""scp -f "%s"""".format(remoteFilenameMask))
      val sin = new BufferedInputStream(ch.getInputStream())
      val sout = ch.getOutputStream()
      ch.connect(ssh.options.connectTimeout.toInt)
      
      sout.write(0)
      sout.flush()
      
      var count=0
      val buf = new StringBuilder() // Warning : Mutable state, take care
      def bufAppend(x:Int) {buf.append(x.asInstanceOf[Char])}
      def bufReset() {buf.setLength(0)}
      def bufStr = buf.toString
      
      while(checkAck(sin)=='C') {
        val fileRights = new Array[Byte](5)
        sin.read(fileRights, 0, 5)

        bufReset()
        Stream.continually(sin.read()).takeWhile(_!=' ').foreach(bufAppend(_))
        val fz = bufStr.toLong

        bufReset()
        Stream.continually(sin.read()).takeWhile(_!=0x0a).foreach(bufAppend(_))
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
        
        var  writtenBytes=0L
        while(writtenBytes<fz) {
          val c=sin.read()
          if (c>=0) {
            fos.write(c)
            writtenBytes+=1
          }
        }
        
        
        fos.close
        
        count+=1
        
        checkAck(sin)
        sout.write(0)
        sout.flush()
      }
      
      count
    } finally {
      if (ch.isConnected) ch.disconnect
    }
  }

 
  
  private def checkAck(in:InputStream):Int = {
    def consumeMessage() = {
      val sb = new StringBuffer()
      Stream.continually(in.read())
      .takeWhile(x => (x != '\n') && (x != -1))
      .foreach(x => sb.append(x.asInstanceOf[Char]))
    }
    in.read() match {
      case 1  => throw new RuntimeException("SSH transfert protocol error "+consumeMessage())
      case 2  => throw new RuntimeException("SSH transfert protocol fatal error "+consumeMessage())
      case x  => x
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

  override def execute(cmds: SSHBatch):List[String] =  cmds.cmdList.map(execute(_))

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
  override def promptPassphrase(message: String) = false
  override def promptYesNo(message: String) = true
  override def showMessage(message: String) = {}
  override def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = Array(getPassword())
}

// ==========================================================================================

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