package fr.janalyse

package object ssh {
  implicit def stringToCommand(cmd: String) = new SSHCommand(cmd)
  implicit def stringListToBatchList(cmdList: List[String]) = new SSHBatch(cmdList)
  implicit def stringToRemoteFile(filename: String) = new SSHRemoteFile(filename)

}
