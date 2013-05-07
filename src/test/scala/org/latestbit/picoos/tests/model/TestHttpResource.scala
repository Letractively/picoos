/*
   Copyright 2013 Abdulla Abdurakhmanov (abdulla@latestbit.com)
   Original sources are available at www.latestbit.com

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package org.latestbit.picoos.tests.model

import org.latestbit.picoos._
import org.latestbit.picoos.dsl._

case class TestJson(value : String)
class TestHttpResource extends HttpResource("/resource1") {

  def getUsers = apiMethod as { 
    httpOkResult
  }
  
  def getUsers2 = apiMethod("/getUsers2Mapped") as { 
    httpTextResult("Hello!")
  }
  
  def getUsers3 = apiMethod as { req : HttpResourceRequest => 
    httpJsonResult(TestJson("Test"))
  }

  def getUsers4 = apiMethod("/getUsers4", HttpMethod.GET) as { req : HttpResourceRequest => 
    httpXmlResult(
        <test>xml result</test>
    )
  }
  
  def getUsers5= apiMethod(HttpMethod.GET) as { req : HttpResourceRequest => 
    httpOkResult
  }
  
  def getUsers6= apiMethod(HttpMethod.GET) as { (req : HttpResourceRequest, resp : HttpResourceResponse) => 
    httpOkResult
  }    

  def getUsers7= apiMethod(HttpMethod.GET) as { (req : HttpResourceRequest, resp : HttpResourceResponse) => 
    httpOkResult(CachingStrategy(false, false))
  }    
  
  def getUsersSecure4 = restrictedApiMethod("/getUsersSecure4", HttpMethod.GET) as { req : HttpResourceRequest => 
    httpXmlResult(
        <test>xml result</test>
    )
  }
  
}

class TestProxyResources extends HttpProxyResource("/web") {
  
  def ttt = apiMethod as { req : HttpResourceRequest  =>
    httpTextResult("Hello from Picoos! " + req.servicePath )
  }
  
  override def dispatch(req : HttpResourceRequest, resp : HttpResourceResponse ) : Boolean = {        
    val dispatchPath = "/html"+ req.servicePath.replace( this.resourcePath, "") +".html"
    req.http.getRequestDispatcher(dispatchPath).forward(req.http, resp.http)
    true
  }
}

class TestCanonicalRestful extends HttpCanonicalResource("/cres") {
  
  def ttt = apiMethod as { req : HttpResourceRequest  =>
    httpTextResult("Hello from Picoos! " + req.servicePath )
  }
  
  override def $list = apiMethod as{
    httpJsonResult(TestJson("Test") :: Nil)    
  }	

  override def $newResource = apiMethod as {
    httpOkResult
  }

  override def $getResource( resourceId : String) = apiMethod as {
    httpJsonResult(TestJson("Test"))
  } 

  override def $replaceResource( resourceId : String) = apiMethod as {
    httpOkResult
  }	

  override def $deleteResource( resourceId : String) = apiMethod as {
    httpOkResult
  }
  
}
