package fr.janalyse.ssh

import scala.util.{ Properties => SP }
import java.io.File.{ separator => FS, pathSeparator => PS }


/**
 * SSHOptions stores all ssh parameters
 * @author David Crosson
 */
case class SSHOptions(
  username: String = util.Properties.userName,
  password: SSHPassword = NoPassword,
  passphrase: SSHPassword = NoPassword,
  name: Option[String] = None,
  port: Int = 22,
  prompt: Option[String] = None,
  timeout: Long = 0,
  connectTimeout: Long = 30000,
  retryCount: Int = 5,
  retryDelay: Int = 2000,
  sshUserDir: String = SP.userHome + FS + ".ssh",
  sshKeyFile: Option[String] = None, // if None, will look for default names. (sshUserDir is used) 
  charset: String = "ISO-8859-15",
  noneCipher: Boolean = true,
  compress: Option[Int] = None,
  execWithPty:Boolean = false    // Sometime some command doesn't behave the same with or without tty, cf mysql
  )(
    val host: String = "localhost") {
  val keyfiles2lookup = sshKeyFile ++ List("id_rsa", "id_dsa") // ssh key search order (from sshUserDir) 
}
