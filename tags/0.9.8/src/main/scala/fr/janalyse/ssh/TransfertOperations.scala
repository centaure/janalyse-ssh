package fr.janalyse.ssh

import java.io.File

/**
 * TransfertOperations defines generic data transfer operations over SCP or SFTP
 */
trait TransfertOperations extends CommonOperations {
  /**
   * get remote file content as an optional String
   * @param remoteFilename file content to get
   * @return Some content or None if file was not found
   */
  def get(remoteFilename: String): Option[String]

  /**
   * get remote file content as an optional bytes array
   * @param remoteFilename file content to get
   * @return Some content or None if file was not found
   */
  def getBytes(remoteFilename: String): Option[Array[Byte]]

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
   * Copy a remote file to a local one using the same filename
   * @param filename file name
   */
  def receive(filename: String) {
    receive(filename, new File(filename))
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
   * Copy a local file to a remote one using the same name
   * @param filename file name
   */
  def send(filename: String) {
    send(new File(filename), filename)
  }

  /**
   * Copy a local file to a remote one
   * @param fromLocalFile Source file (local system)
   * @param remoteDestination Destination file name (on remote system)
   */
  def send(fromLocalFile: File, remoteDestination: String)

}
