package org.latestbit.picoos

object HttpFormatter {
  
  	def encodeParams(params : Map[String, String]) : String = {
	  params.foldLeft("")((all, item) => all+"&"+urlEncode(item._1)+"="+urlEncode(item._2))
	}
  	
  	def decodeParams(params : String) : Map[String, String] = {
  	  params.split("&").toList.filter(_.length() >0).map { item =>
  	    val param = item.split("=")
  	    if(param.size > 1)
  	    	(param(0) -> urlDecode(param(1)))
  	    else
  	    	(param(0) -> "")
  	  }.toMap
  	}
	
	def urlEncode(url : String, encoding : String = "UTF-8") : String = java.net.URLEncoder.encode(url, encoding)
	def urlEncodeOnlyNonAscii(url : String, skipParams : Boolean = true, encoding : String = "UTF-8") : String = {
	  val str = skipParams match {
	    case true if(url.contains("?")) => url.substring(0, url.indexOf("?"))
	    case _ => url
	  }
	  urlEncode(str,encoding).replace("%3A",":").replace("%2F","/").replace("%3F","?").replace("%3D","=").replace("%26","&").replace("+"," ")
	}
	
	def urlDecode(url : String, encoding : String = "UTF-8") : String = java.net.URLDecoder.decode(url, encoding)
	
	def formatUrlWithParams(url : String, params : Map[String, String]) =
	  url+"?"+encodeParams(params).substring(1)
}