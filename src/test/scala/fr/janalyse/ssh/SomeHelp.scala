package fr.janalyse.ssh

trait SomeHelp {
  def f(filename: String) = new java.io.File(filename)
  def howLongFor[T](what: => T) = {
    val begin = System.currentTimeMillis()
    val result = what
    val end = System.currentTimeMillis()
    (end - begin, result)
  }

}