package org.latestbit.picoos.client

import java.net.URL
import scala.collection.JavaConversions._
import java.io.OutputStreamWriter
import org.latestbit.picoos.serializers.JSonSerializer
import java.net.HttpURLConnection
import org.latestbit.picoos.PerfUtils
import java.util.logging.Logger
import java.util.logging.Level
import org.latestbit.picoos.HttpFormatter


case class RestClientResult[T](val body : T, httpResponseCode : Int, httpResponseMessage : String)

class RestClient(
    val userAgent : String = "PicoosRESTClient", 
    val encoding : String = "UTF-8", 
    connectionTimeoutMs : Int = 10000) {
	
    private final val log : Logger  = Logger.getLogger(classOf[RestClient].getName())
  
	def httpGet(url: String, headers : Map[String,String] = Map()) : RestClientResult[String] = {
	  PerfUtils.meausureTime {	    	
		  val connection : HttpURLConnection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
		  connection.setRequestProperty("User-Agent", userAgent)
		  connection.setConnectTimeout(connectionTimeoutMs)
		  setupHeaders(connection, headers)
		  connection.connect()
		  
		  RestClientResult[String](
		      scala.io.Source.fromInputStream(connection.getInputStream(), encoding).getLines().mkString("\n"),
		      connection.getResponseCode(),
		      connection.getResponseMessage()
		  )
	  }(time => log.logp(Level.FINE, classOf[RestClient].getName(), "httpGet", s"The request to $url has been processed within $time ms."))
	}
	
	def httpGetJSon[T : Manifest](url: String, headers : Map[String,String] = Map()) : RestClientResult[T] = {
	  val strResult = httpGet(url, headers)
	  RestClientResult[T](
			  JSonSerializer.deserialize[T](strResult.body),
			  strResult.httpResponseCode,
			  strResult.httpResponseMessage
	  )			  
	}
	
	def httpPost(url: String, data: Map[String, String], headers : Map[String,String] = Map()) : RestClientResult[String] = {
	  httpExchangeData(url, "POST", HttpFormatter.encodeParams(data), headers)
	}
	
	def httpPostAndGetJSon[T : Manifest](url: String, data: Map[String, String], headers : Map[String,String] = Map() ) : RestClientResult[T] = {
	  val strResult = httpPost(url, data, headers)
	  RestClientResult[T](
	      JSonSerializer.deserialize[T](strResult.body),
	      strResult.httpResponseCode,
	      strResult.httpResponseMessage
	  )
	}
	
	def httpPostAndGetData(url: String, data: Map[String, String], headers : Map[String,String] = Map()) : RestClientResult[Map[String, String]] = {
	  val strResult = httpPost(url, data, headers)
	  RestClientResult[Map[String, String]](
	      HttpFormatter.decodeParams(strResult.body),
	      strResult.httpResponseCode,
	      strResult.httpResponseMessage
	  )
	}
	
	def httpPut(url: String, data: Map[String, String], headers : Map[String,String] = Map()) : RestClientResult[String] = {
	  httpExchangeData(url, "PUT", HttpFormatter.encodeParams(data), headers)
	}
	
	def httpPutAndGetJSon[T : Manifest](url: String, data: Map[String, String], headers : Map[String,String] = Map()) : RestClientResult[T] = {
	  val strResult = httpPut(url, data, headers)
	  RestClientResult[T](
	      JSonSerializer.deserialize[T](strResult.body),
	      strResult.httpResponseCode,
	      strResult.httpResponseMessage
	  )
	}
	
	protected def setupHeaders(connection : HttpURLConnection, headers : Map[String,String]) = {
	  headers.foreach(item => {
	    connection.setRequestProperty(item._1, item._2)
	  })
	}
	
	def httpExchangeData(url : String, httpMethod : String, inputData : String, headers : Map[String,String] = Map() ) : RestClientResult[String] = {
	  PerfUtils.meausureTime {
		  val connection : HttpURLConnection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
		  connection.setRequestProperty("User-Agent", userAgent)
		  connection.setConnectTimeout(connectionTimeoutMs)
		  connection.setDoOutput(true)
		  connection.setRequestMethod(httpMethod)
		  setupHeaders(connection, headers)
		  connection.connect()
		  
		  val output = new OutputStreamWriter(connection.getOutputStream())
	      output.write(inputData)
	      output.flush
	      output.close      
	
	      val result = RestClientResult(
	          scala.io.Source.fromInputStream(connection.getInputStream(), encoding).getLines().mkString("\n"),
	          connection.getResponseCode(),
	          connection.getResponseMessage()
	      )
	      connection.disconnect()
	      result
	   }(time => log.logp(Level.FINE,classOf[RestClient].getName(), "httpExchangeData", s"The request to $url has been processed within $time ms."))
	}
}
