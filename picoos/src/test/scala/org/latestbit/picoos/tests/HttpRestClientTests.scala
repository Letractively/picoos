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
      
      val ipInfo : IPInfo = restClient.httpGetJSon[IPInfo]("http://httpbin.org/ip")
      assert ( ipInfo.origin!=null )
      assert ( ipInfo.origin.length() > 0 )
      
      val userAgent : UserAgentJson = restClient.httpGetJSon[UserAgentJson]("http://httpbin.org/user-agent")
      assert ( userAgent.`user-agent`!=null )
      assert ( userAgent.`user-agent`.equalsIgnoreCase("TestPicoosClient") )
    }
  }

}