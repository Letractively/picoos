package org.latestbit.picoos.client

import java.net.URL
import scala.collection.JavaConversions._
import java.io.OutputStreamWriter
import org.latestbit.picoos.serializers.JSonSerializer

class RestClient(val userAgent : String = "PicoosRESTClient", val encoding : String = "UTF-8", connectionTimeoutMs : Int = 10000) {
	
	def httpGet(url: String) : String = {
	  val connection = new URL(url).openConnection()
	  connection.setRequestProperty("User-Agent", userAgent)
	  connection.setConnectTimeout(connectionTimeoutMs)
	  connection.connect()
	  scala.io.Source.fromInputStream(connection.getInputStream()).getLines().mkString("\n")
	}
	
	def httpGetJSon[T : Manifest](url: String) : T = {
	  JSonSerializer.deserialize[T](httpGet(url))
	}
	
	def httpPost(url: String, data: Map[String, String]) = {
	  val connection = new URL(url).openConnection()
	  connection.setRequestProperty("User-Agent", userAgent)
	  connection.setConnectTimeout(connectionTimeoutMs)
	  connection.setDoOutput(true)
	  connection.connect()
	  
	  val output = new OutputStreamWriter(connection.getOutputStream())
      output.write(encodePostParameters(data))
      output.flush
      output.close      

      scala.io.Source.fromInputStream(connection.getInputStream()).getLines().mkString("\n")
	}
	
	def encodePostParameters(params : Map[String, String]) : String = {
	  params.foldLeft("")((all, item) => all+"&"+urlEncode(item._1)+"="+urlEncode(item._2))
	}
	
	def urlEncode(str : String) : String = java.net.URLEncoder.encode(str, encoding)
}