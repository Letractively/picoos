package org.latestbit.picoos.tests

import org.scalatest.FeatureSpec
import org.latestbit.picoos.client._
import scala.reflect._
import org.latestbit.picoos.serializers.JSonSerializer
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import org.latestbit.picoos.HttpFormatter
import scala.collection.JavaConverters._


case class IPInfo(origin: String)
case class UserAgentJson(`user-agent` : String)

@JsonIgnoreProperties(ignoreUnknown = true)
case class IPInfo2(origin : String, unexistsQQQQ : String)

@JsonIgnoreProperties(ignoreUnknown = true)
class IPInfo3(var origin: String = null)

@JsonIgnoreProperties(ignoreUnknown = true)
class IPInfo4 { var origin: String  = null }

class HttpRestClientTests extends FeatureSpec {
  feature("RESTClient functionality tests") {
    val restClient = new RestClient("TestPicoosClient")
    scenario("Check GET") {
      
      val ipInfo : IPInfo = restClient.httpGetJSon[IPInfo]("http://httpbin.org/ip").body
      assert ( ipInfo.origin!=null )
      assert ( ipInfo.origin.length() > 0 )
      
      val getInfo = restClient.httpGetJSon[Map[String, Any]](
          HttpFormatter.formatUrlWithParams(
              "http://httpbin.org/get",
              Map("fdgg" -> "ggg", "sss"-> "hhh")
          )
      ).body
      assert ( getInfo!=null )
      assert ( getInfo.get("args").get.asInstanceOf[Map[String, Any]].size == 2)
      
      val ipInfo2 : IPInfo2 = restClient.httpGetJSon[IPInfo2](
          HttpFormatter.formatUrlWithParams(
              "http://httpbin.org/get",
              Map("fdgg" -> "ggg", "sss"-> "hhh")
          )
      ).body
      assert ( ipInfo2!=null )
      assert ( ipInfo2.origin.length() > 0)
      
      /*val ipInfo3 : IPInfo3 = restClient.httpGetJSon[IPInfo3](
          HttpFormatter.formatUrlWithParams(
              "http://httpbin.org/get",
              Map("fdgg" -> "ggg", "sss"-> "hhh")
          )
      ).body
      assert ( ipInfo3!=null )
      assert ( ipInfo3.origin.length() > 0)*/
      
      val ipInfo4 : IPInfo4 = restClient.httpGetJSon[IPInfo4](
          HttpFormatter.formatUrlWithParams(
              "http://httpbin.org/get",
              Map("fdgg" -> "ggg", "sss"-> "hhh")
          )
      ).body
      assert ( ipInfo4!=null )
      assert ( ipInfo4.origin.length() > 0)
      
      val userAgent : UserAgentJson = restClient.httpGetJSon[UserAgentJson]("http://httpbin.org/user-agent").body
      assert ( userAgent.`user-agent`!=null )
      assert ( userAgent.`user-agent`.equalsIgnoreCase("TestPicoosClient") )
      
      
      val postRes = restClient.httpPost("http://httpbin.org/post", Map("test"->"value") ).body
      assert (postRes!=null)
      assert (postRes.length() > 0)
      
      val postResJSon = restClient.httpPostAndGetJSon[Map[String, Any]]("http://httpbin.org/post", Map("test"->"value")).body
      assert (postResJSon!=null)
      assert (postResJSon.get("headers").isDefined)
      assert (postResJSon.get("headers").get.asInstanceOf[Map[String,Any]].get("User-Agent") == Some("TestPicoosClient"))
      
      val decodedParams = HttpFormatter.decodeParams("&test=value&test2=value2")
      assert (decodedParams.size == 2)
      assert (decodedParams.get("test").isDefined)
      assert (decodedParams.get("test") == Some("value"))
      assert (decodedParams.get("test2").isDefined)
    }
  }
  
  feature("HttpFormatter functionality tests") {
    val testUrl = "http://www.example.net/test/тест本 /test4/?test=тест本&test2=тест本";
    assert ( HttpFormatter.urlDecode(HttpFormatter.urlEncode(testUrl)).equals(testUrl))
    val urlWithAscii = HttpFormatter.urlEncodeOnlyNonAscii(testUrl, false)
    assert ( urlWithAscii.contains("http://"))
    assert ( urlWithAscii.contains("&test2="))
    assert ( urlWithAscii.contains("/test/"))
    
    val partUrlWithAscii = HttpFormatter.urlEncodeOnlyNonAscii(testUrl)
    assert ( partUrlWithAscii.contains("http://"))
    assert ( partUrlWithAscii.contains("/test/"))
    println(partUrlWithAscii)
    
    val testUrl2 = "http://www.example.net/test/тест本";
    val partUrlWithAscii2 = HttpFormatter.urlEncodeOnlyNonAscii(testUrl2)
    assert ( partUrlWithAscii.contains("http://"))
    assert ( partUrlWithAscii.contains("/test/"))
  }
}
