package org.latestbit.picoos.tests

import org.scalatest.FeatureSpec
import org.latestbit.picoos.client._
import scala.reflect._
import org.latestbit.picoos.serializers.JSonSerializer


case class IPInfo(origin: String)
case class UserAgentJson(`user-agent` : String)

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
      println(getInfo.get("args").get.asInstanceOf[collection.mutable.Map[String, Any]])
      assert ( getInfo.get("args").get.asInstanceOf[collection.mutable.Map[String, Any]].size == 2)
      
      val userAgent : UserAgentJson = restClient.httpGetJSon[UserAgentJson]("http://httpbin.org/user-agent").body
      assert ( userAgent.`user-agent`!=null )
      assert ( userAgent.`user-agent`.equalsIgnoreCase("TestPicoosClient") )
      
      
      val postRes = restClient.httpPost("http://httpbin.org/post", Map("test"->"value")).body
      assert (postRes!=null)
      assert (postRes.length() > 0)
      
      val postResJSon = restClient.httpPostAndGetJSon[Map[String, Any]]("http://httpbin.org/post", Map("test"->"value")).body
      assert (postResJSon!=null)
      assert (postResJSon.get("headers").isDefined)
      assert (postResJSon.get("headers").get.asInstanceOf[collection.mutable.Map[String,Any]].get("User-Agent") == Some("TestPicoosClient"))
    }
  }

}