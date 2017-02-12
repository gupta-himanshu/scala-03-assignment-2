object URL {
  def apply(protocol: String, domain: String, path: String, params: Map[String, String]): String = {
    val paramsStr = params.map{case(key, value) => s"$key=$value&"}
    val urlString = s"$protocol://$domain/$path?$paramsStr"
    urlString
  }

  def unapply(url: String): Option[(String, String, String, Map[String, String])] = {
    val protocolAndRest = url.split("://")
    val protocol = protocolAndRest.headOption
    val urlWithoutProtocol = protocolAndRest.tail.flatMap(_.split("\\?"))
    val domainWithPath = urlWithoutProtocol.headOption
    val domain = domainWithPath.flatMap(_.split("/").headOption)
    val path = domainWithPath.map(path => "/" + path.split("/").tail.mkString("/"))
    val paramsMap = 
      if(urlWithoutProtocol.length > 0) {
        urlWithoutProtocol.tail.flatMap(_.split("&")).map(param => (param.split("=").headOption, param.split("=").tail.headOption)).toMap
      } else {
        Map()
      }

    val params = paramsMap.map{case(key, value) => (key.getOrElse(""), value.getOrElse(""))}

    for{
      protocol <- protocol
      domain <- domain
      path <- path
      params <- Some(params)
    } yield(protocol, domain, path, params)
  }
}

object UrlExtractor {
  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      println("Please provide a valid string")
      sys.exit(0)
    }

    args(0) match {
        case URL(protocol, domain, path, params) => println("Protocol: " + protocol + "\nDomain: " + domain + "\nPath: " + path + "\nParams: " + params)
        case _ => println("No Match Found")
    }
  }
}
