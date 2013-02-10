#!/bin/sh
exec java -jar jassh.jar -nocompdaemon -usejavacp -savecompiled "$0" "$@"
!#

import jassh._

import scala.concurrent._
import scala.concurrent.duration._
import duration._
import scala.util._
import scala.annotation.tailrec


import java.util.concurrent.{Executors,ThreadPoolExecutor,TimeUnit}
implicit val customEC = ExecutionContext.fromExecutorService(
  Executors.newCachedThreadPool() match {
    case e:ThreadPoolExecutor => 
      //Too allow a quick exit from this script 
      // because default value is 60s
      e.setKeepAliveTime(1, TimeUnit.SECONDS)
      e
    case x => x
  }
)

val remotehosts = (1 to 10) map { num =>
  SSHOptions(username="test", password="testtest", name=Some(s"host#$num"))("127.0.0.1")
}


val futuresResults = remotehosts.map { rh =>
  future {
    SSH.once(rh) { ssh =>
      ssh.execute(s"""echo 'Hello from ${rh.name getOrElse "default"}'""")
    }
  }
}

Future.sequence(futuresResults) onComplete { _ match {
    case Failure(err) => println(err.getMessage)
    case Success(messages) => println(messages.size)
  }
}
