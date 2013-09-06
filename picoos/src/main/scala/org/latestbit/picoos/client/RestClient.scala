package org.latestbit.picoos.client

import java.net.URL
import scala.collection.JavaConversions._
import java.io.OutputStreamWriter
import org.latestbit.picoos.serializers.JSonSerializer
import java.net.HttpURLConnection
import org.latestbit.picoos.PerfUtils
import java.util.logging.Logger
import java.util.logging.Level

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

case class RestClientResult[T](val body : T, httpResponseCode : Int, httpResponseMessage : String)

class RestClient(
    val userAgent : String = "PicoosRESTClient", 
    val encoding : String = "UTF-8", 
    connectionTimeoutMs : Int = 10000) {
	
    private final val log : Logger  = Logger.getLogger(classOf[RestClient].getName())
  
	def httpGet(url: String) : RestClientResult[String] = {
	  PerfUtils.meausureTime {	    	
		  val connection : HttpURLConnection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
		  connection.setRequestProperty("User-Agent", userAgent)
		  connection.setConnectTimeout(connectionTimeoutMs)
		  connection.connect()
		  
		  RestClientResult[String](
		      scala.io.Source.fromInputStream(connection.getInputStream()).getLines().mkString("\n"),
		      connection.getResponseCode(),
		      connection.getResponseMessage()
		  )
	  }(time => log.logp(Level.FINE, classOf[RestClient].getName(), "httpGet", s"The request to $url has been processed within $time ms."))
	}
	
	def httpGetJSon[T : Manifest](url: String) : RestClientResult[T] = {
	  val strResult = httpGet(url)
	  RestClientResult[T](
			  JSonSerializer.deserialize[T](strResult.body),
			  strResult.httpResponseCode,
			  strResult.httpResponseMessage
	  )			  
	}
	
	def httpPost(url: String, data: Map[String, String]) : RestClientResult[String] = {
	  httpExchangeData(url, "POST", HttpFormatter.encodeParams(data))
	}
	
	def httpPostAndGetJSon[T : Manifest](url: String, data: Map[String, String]) : RestClientResult[T] = {
	  val strResult = httpPost(url, data)
	  RestClientResult[T](
	      JSonSerializer.deserialize[T](strResult.body),
	      strResult.httpResponseCode,
	      strResult.httpResponseMessage
	  )
	}
	
	def httpPostAndGetData(url: String, data: Map[String, String]) : RestClientResult[Map[String, String]] = {
	  val strResult = httpPost(url, data)
	  RestClientResult[Map[String, String]](
	      HttpFormatter.decodeParams(strResult.body),
	      strResult.httpResponseCode,
	      strResult.httpResponseMessage
	  )
	}
	
	def httpPut(url: String, data: Map[String, String]) : RestClientResult[String] = {
	  httpExchangeData(url, "PUT", HttpFormatter.encodeParams(data))
	}
	
	def httpPutAndGetJSon[T : Manifest](url: String, data: Map[String, String]) : RestClientResult[T] = {
	  val strResult = httpPut(url, data)
	  RestClientResult[T](
	      JSonSerializer.deserialize[T](strResult.body),
	      strResult.httpResponseCode,
	      strResult.httpResponseMessage
	  )
	}
	
	def httpExchangeData(url : String, httpMethod : String, inputData : String) : RestClientResult[String] = {
	  PerfUtils.meausureTime {
		  val connection : HttpURLConnection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
		  connection.setRequestProperty("User-Agent", userAgent)
		  connection.setConnectTimeout(connectionTimeoutMs)
		  connection.setDoOutput(true)
		  connection.setRequestMethod(httpMethod)
		  connection.connect()
		  
		  val output = new OutputStreamWriter(connection.getOutputStream())
	      output.write(inputData)
	      output.flush
	      output.close      
	
	      val result = RestClientResult(
	          scala.io.Source.fromInputStream(connection.getInputStream()).getLines().mkString("\n"),
	          connection.getResponseCode(),
	          connection.getResponseMessage()
	      )
	      connection.disconnect()
	      result
	   }(time => log.logp(Level.FINE,classOf[RestClient].getName(), "httpExchangeData", s"The request to $url has been processed within $time ms."))
	}
}
