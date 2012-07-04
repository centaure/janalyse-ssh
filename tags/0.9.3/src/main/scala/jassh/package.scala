package object jassh {
  type SSH = fr.janalyse.ssh.SSH
  val SSH = fr.janalyse.ssh.SSH
  type SSHOptions = fr.janalyse.ssh.SSHOptions
  val SSHOptions = fr.janalyse.ssh.SSHOptions
  //implicit def stringToCommand(cmd: String) = new fr.janalyse.ssh.SSHCommand(cmd)
  //implicit def stringListToBatchList(cmdList: Iterable[String]) = new fr.janalyse.ssh.SSHBatch(cmdList)
  //implicit def stringToRemoteFile(filename: String) = new fr.janalyse.ssh.SSHRemoteFile(filename)
}
