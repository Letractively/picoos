package org.latestbit.picoos

object HttpFormatter {
  
  	def encodeParams(params : Map[String, String]) : String = {
	  params.foldLeft("")((all, item) => all+"&"+urlEncode(item._1)+"="+urlEncode(item._2))
	}
  	
  	def decodeParams(params : String) : Map[String, String] = {
  	  params.split("&").toList.filter(_.length() >0).map { item =>
  	    val param = item.split("=")
  	    if(param.size > 1)
  	    	(param(0) -> param(1))
  	    else
  	    	(param(0) -> "")
  	  }.toMap
  	}
	
	def urlEncode(str : String, encoding : String = "UTF-8") : String = java.net.URLEncoder.encode(str, encoding)
	def urlEncodeOnlyNonAscii(str : String, encoding : String = "UTF-8") : String = {
	  urlEncode(str,encoding).replace("%3A",":").replace("%2F","/").replace("%3F","?").replace("%3D","=").replace("%26","&")
	}
	
	def urlDecode(str : String, encoding : String = "UTF-8") : String = java.net.URLDecoder.decode(str, encoding)
	
	def formatUrlWithParams(url : String, params : Map[String, String]) =
	  url+"?"+encodeParams(params).substring(1)
}