#!/bin/sh
exec java -jar ../target/janalyse-ssh.jar -nocompdaemon -usejavacp -savecompiled "$0" "$@"
!#

import fr.janalyse.ssh._

SSH.connect(host="localhost", username="test") { ssh =>
  println(ssh.execute("""echo -n "Hello World from `hostname`" """))
}

