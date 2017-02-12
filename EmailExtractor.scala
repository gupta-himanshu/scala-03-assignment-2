object EmailExtractor {
  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      println("Please provide a valid email")
      sys.exit(0)
    } 

    val EMAIL = """([\w\.]+)@([a-zA-Z0-9.]+)""".r
    args(0) match {
      case EMAIL(user, domain) => println(s"User = $user   Domain = $domain")
      case _ => println("Not a valid Email")
    }
  }
}
