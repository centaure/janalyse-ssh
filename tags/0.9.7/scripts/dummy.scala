#!/bin/sh
exec java -jar jassh.jar -nocompdaemon -usejavacp -savecompiled "$0" "$@"
!#
jassh.SSH.shell("localhost", "test", "testtest") { sh =>
  print(sh.execute("""echo "Hello World from `hostname`" """))
}