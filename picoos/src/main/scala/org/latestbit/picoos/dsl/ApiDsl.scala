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

package org.latestbit.picoos.dsl

import org.latestbit.picoos._
import org.latestbit.picoos.HttpMethod._
import scala.xml.Node
import org.latestbit.picoos.serializers._
import scala.collection.immutable.Map


case class CachingOptions(val noCacheMode : Boolean = false, val privateCacheMode : Boolean = true) {
  def proceedOptions(resp : HttpResourceResponse) = {
    if(noCacheMode) {
      resp.http.setHeader("Cache-Control",  "no-cache, no-store, must-revalidate")
      resp.http.setHeader("Pragma",  "no-cache")
      resp.http.setDateHeader("Expires",  -1)
    }
    if(privateCacheMode) {
      resp.http.setHeader("Cache-Control", "private")
    }
  }
}

abstract class RestMethodResult(val cacheFlags : CachingOptions) {
  def proceedHttpResponse(resp : HttpResourceResponse) = {
    cacheFlags.proceedOptions(resp)
  }
}

case class httpOkResult(override val cacheFlags : CachingOptions= CachingOptions()) extends RestMethodResult(cacheFlags)
object httpOkResult extends httpOkResult(CachingOptions())
case class httpNoResult(override val cacheFlags : CachingOptions= CachingOptions()) extends RestMethodResult(cacheFlags)
object httpNoResult extends httpNoResult(CachingOptions())

case class httpTextResult(textResult : String, charset: String = "UTF-8", override val cacheFlags : CachingOptions= CachingOptions()) extends RestMethodResult(cacheFlags) {
  override def proceedHttpResponse(resp : HttpResourceResponse) = {
	   super.proceedHttpResponse(resp)
	   resp.http.setContentType("text/plain; charset="+charset)
	   resp.http.getOutputStream().write(textResult.getBytes(charset))
	   resp.http.getOutputStream().flush()
	   //resp.http.getWriter().flush()	   
  }
} 
case class httpJsonResult[T<:AnyRef](jsonObj : T, charset: String = "UTF-8", override val cacheFlags : CachingOptions= CachingOptions()) extends RestMethodResult(cacheFlags) {
  //lazy val textResult = Json.generate(jsonObj)
  val textResult = JSonSerializer.serialize(jsonObj)

  override def proceedHttpResponse(resp : HttpResourceResponse) = {
	  super.proceedHttpResponse(resp)
      resp.http.setContentType("application/json; charset="+charset)
      resp.http.getOutputStream().write(textResult.getBytes(charset))
	  resp.http.getOutputStream().flush()
  }  
}

case class httpXmlResult(xmlObj : Node, charset: String = "UTF-8", override val cacheFlags : CachingOptions= CachingOptions()) extends RestMethodResult(cacheFlags)  {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
	  super.proceedHttpResponse(resp)
      resp.http.setContentType("text/xml")
      scala.xml.XML.write(resp.http.getWriter(), xmlObj, charset, true, null)
      resp.http.getWriter().flush()
	}  
}
case class httpRedirectResult(url : String, override val cacheFlags : CachingOptions= CachingOptions())  extends RestMethodResult(cacheFlags) {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
		super.proceedHttpResponse(resp)	  
		resp.http.sendRedirect( url )	  
	}
}

case class httpErrorResult(errorCode : Int, errorString : String, override val cacheFlags : CachingOptions= CachingOptions()) extends RestMethodResult(cacheFlags) {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
		super.proceedHttpResponse(resp)
		resp.http.sendError(errorCode, errorString)	  
	}
}

class AuthParams(val permissions : Seq[String], val authFunction : Option[( (String, HttpResourceRequest) => Boolean )])
class RestParams(val path : String = null, val httpMethod : HttpMethod = HttpMethod.ANY_METHOD)

class RestMethodBodyDef {
  
  abstract class RestMethodBodyHandler
  class RestMethodBodyHandlerNoParams(inBody : => RestMethodResult) extends RestMethodBodyHandler {
	  def handler = {
	    inBody
	  } 
  }	
  class RestMethodBodyHandlerHttpRequest(val handler : (HttpResourceRequest)=> RestMethodResult) extends RestMethodBodyHandler
  class RestMethodBodyHandlerHttpRequestAndResponse(val handler : (HttpResourceRequest, HttpResourceResponse)=> RestMethodResult) extends RestMethodBodyHandler
	  
  var bodyHandler : Option[ RestMethodBodyHandler ] = None	  
  var authParams : Option[AuthParams] = None	  
  var restParams : RestParams = new RestParams()  
  
  def withAuthParams(params : AuthParams) = {
    this.authParams = Some(params)
    this
  }
  
  def withRestParams(params : RestParams) = {
    this.restParams = params
    this
  }
  
  def withBody(body : => RestMethodResult) = {
    bodyHandler = Some(new RestMethodBodyHandlerNoParams(body))
    this
  }
  
  def withBody(body : (HttpResourceRequest)=> RestMethodResult) = {
    bodyHandler = Some(new RestMethodBodyHandlerHttpRequest(body))
    this
  }
  
  def withBody(body : (HttpResourceRequest, HttpResourceResponse)=> RestMethodResult) = {
    bodyHandler = Some(new RestMethodBodyHandlerHttpRequestAndResponse(body))
    this
  }
  
  def execute() = {
	  bodyHandler match {
	      case Some(body : RestMethodBodyHandlerNoParams) => body.handler
	      case _ => throw new Exception("Unable to execute empty or wrong request handler!")
	  }
  }
	
  def execute(req : HttpResourceRequest) = {
	  bodyHandler match {
      	case Some(body : RestMethodBodyHandlerNoParams) => body.handler
      	case Some(body : RestMethodBodyHandlerHttpRequest) => body.handler(req)
      	case _ => throw new Exception("Unable to execute empty or wrong request handler!")
  	  }
  }

  def execute(req : HttpResourceRequest, resp : HttpResourceResponse) = {
	  bodyHandler match {
	      case Some(body : RestMethodBodyHandlerNoParams) => body.handler
	      case Some(body : RestMethodBodyHandlerHttpRequest) => body.handler(req)
	      case Some(body : RestMethodBodyHandlerHttpRequestAndResponse) => body.handler(req, resp)
	      case _ => throw new Exception("Unable to execute empty or wrong request handler!")
	  }  
  }  
}

trait ApiDsl {
	
	object as {
	  def apply (body : => RestMethodResult) : RestMethodBodyDef = { new RestMethodBodyDef().withBody(body) }
	  def apply( body : (HttpResourceRequest)=> RestMethodResult) : RestMethodBodyDef = { new RestMethodBodyDef().withBody(body) }
	  def apply( body : (HttpResourceRequest, HttpResourceResponse)=> RestMethodResult) : RestMethodBodyDef = { new RestMethodBodyDef().withBody(body) }	  
	}
	
	trait RestMethodTrait {
	  def as( body : => RestMethodResult) : RestMethodBodyDef = { new RestMethodBodyDef().withBody(body) }
	  def as( body : (HttpResourceRequest)=> RestMethodResult) : RestMethodBodyDef = { new RestMethodBodyDef().withBody(body) }
	  def as( body : (HttpResourceRequest, HttpResourceResponse)=> RestMethodResult) : RestMethodBodyDef = { new RestMethodBodyDef().withBody(body) }  
	}
	
	case class restMethod(override val path : String = null, override val httpMethod : HttpMethod = HttpMethod.ANY_METHOD) extends RestParams(path, httpMethod) with RestMethodTrait {	  
	  override def as(body : => RestMethodResult) : RestMethodBodyDef = { super.as(body).withRestParams(this) }
	  override def as(body : (HttpResourceRequest) => RestMethodResult) : RestMethodBodyDef = { super.as(body).withRestParams(this) }
	  override def as(body : (HttpResourceRequest, HttpResourceResponse)=> RestMethodResult) : RestMethodBodyDef = { super.as(body).withRestParams(this) }
	}
	
	case class InternalRestMethodWithAuth (path : String = null, httpMethod : HttpMethod = HttpMethod.ANY_METHOD, authParams : requireAuth) extends RestMethodTrait {
	  val restParams = restMethod(path, httpMethod)
	  override def as(body : => RestMethodResult) : RestMethodBodyDef = { super.as(body).withRestParams(restParams).withAuthParams(authParams) }
	  override def as(body : (HttpResourceRequest) => RestMethodResult) : RestMethodBodyDef = { super.as(body).withRestParams(restParams).withAuthParams(authParams) }
	  override def as(body : (HttpResourceRequest, HttpResourceResponse)=> RestMethodResult) : RestMethodBodyDef = { super.as(body).withRestParams(restParams).withAuthParams(authParams) }	  
	}
	
	object restMethod extends RestMethodTrait 
	
	trait RequireAuthTrait {
	  def restMethod(in : RestMethodBodyDef) : RestMethodBodyDef = { in.withAuthParams(requireAuth()) }
	  def restMethod(path : String = null, httpMethod : HttpMethod = HttpMethod.ANY_METHOD) : RestMethodTrait = { 
	    InternalRestMethodWithAuth(path, httpMethod, requireAuth())
	  }
	}
	
	case class requireAuth( override val permissions : Seq[String] = Seq(), override val authFunction : Option[( (String, HttpResourceRequest) => Boolean )]=None ) extends AuthParams(permissions, authFunction) with RequireAuthTrait {
	  override def restMethod(in : RestMethodBodyDef) : RestMethodBodyDef = { 
			  in.withAuthParams(this) 
	  }
	  
	  override def restMethod(path : String = null, httpMethod : HttpMethod = HttpMethod.ANY_METHOD) : RestMethodTrait = { 
	    InternalRestMethodWithAuth(path, httpMethod, this)
	  }

	}
	
	object requireAuth extends RequireAuthTrait	

}

