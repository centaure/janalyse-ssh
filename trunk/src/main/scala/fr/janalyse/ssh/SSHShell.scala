package fr.janalyse.ssh

import java.io._
import com.jcraft.jsch.{ChannelShell}
import java.util.concurrent.ArrayBlockingQueue


class SSHShell(implicit ssh: SSH) extends ShellOperations {
  val readyMessage = "ready-" + System.currentTimeMillis()
  val defaultPrompt = """-PRMT-: """
  val customPromptGiven = ssh.options.prompt.isDefined
  val prompt = ssh.options.prompt getOrElse defaultPrompt

  val options = ssh.options

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

  override def executeAll(cmds: SSHBatch): Iterable[String] = cmds.cmdList.map(execute(_))

  override def executeWithStatus(cmd: SSHCommand): Tuple2[String,Int] = {
    val result = execute(cmd)
    val rc = executeAndTrim("echo $?").toInt
    (result, rc)
  }

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
    private val readyMessageQuotePrefix="'"+readyMessage
    private val promptEqualPrefix="="+prompt

    private val consumerAppender = new StringBuilder(8192)
    private var searchForPromptIndex = 0
    private val promptSize = prompt.size
    private var lastPromptChar = prompt.last
    def write(b: Int) {
      if (b != 13) { //CR removed... CR is always added by JSCH !!!!
        val ch=b.toChar
        consumerAppender.append(ch) // TODO - Add charset support
        if (!ready) { // We want the response and only the response, not the echoed command, that's why the quote is prefixed
          if ( consumerAppender.endsWith(readyMessage) && 
              !consumerAppender.endsWith(readyMessageQuotePrefix)) {
            // wait for at least some results, will tell us that the ssh cnx is ready
            ready = true
            readyQueue.put("ready")
          }
        } else if (lastPromptChar==ch && consumerAppender.endsWith(prompt) && !consumerAppender.endsWith(promptEqualPrefix)) {
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