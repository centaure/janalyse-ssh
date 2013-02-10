#!/bin/sh
exec java -jar jassh.jar -nocompdaemon -usejavacp -savecompiled "$0" "$@"
!#
// jassh.jar can be downloaded here : http://code.google.com/p/janalyse-ssh/

import jassh._

import concurrent._
import duration._
import scala.util._

val parallelismLevel=if (args.size>0) args(0).toInt else 50

/* For local test : 
 * /etc/ssh/sshd_config
 *    Increase : 
 *       MaxSessions 10  -> 60   (parallelismLevel + delta)
 *       MaxStartups 10  -> 60   (parallelismLevel + delta)
 * 
 * The restart sshd
 */

val remotehosts = (1 to 1000) map { num =>
  SSHOptions(username="test", password="testtest", name=Some(s"host#$num"))("127.0.0.1")
}



// implicit val defaultEC = ExecutionContext.Implicits.global
// For ForkJoinPool JDK >= 7 Required
implicit val customEC = ExecutionContext.fromExecutorService(
    new java.util.concurrent.ForkJoinPool(parallelismLevel)
)


val futuresResults = remotehosts.map { rh =>
  future {
    SSH.once(rh) { ssh =>
      ssh.execute(s"""sleep 1 ; echo 'Hello from ${rh.name getOrElse "default"}'""")
    }
  }
}

//futuresResults.foreach( _ onSuccess { case x:String => println(x)})

val allFuture = Future.sequence(futuresResults)

allFuture onComplete { _ match {
    case Failure(err) => println(err.getMessage)
    case Success(messages) => println(messages.size)
  }
}

Await.ready(allFuture, Duration.Inf) // Mandatory to avoid exit before the end of processing
