package fr.janalyse.ssh

sealed trait OS

object Linux  extends OS
object AIX    extends OS
object Darwin extends OS
object SunOS  extends OS
