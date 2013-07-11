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


case class CachingStrategy(val noCacheMode : Boolean = false, val privateCacheMode : Boolean = true)

abstract class ApiMethodResult(val cacheFlags : CachingStrategy) {
  def proceedHttpResponse(resp : HttpResourceResponse) = {
    if(cacheFlags.noCacheMode) {
      resp.http.setHeader("Cache-Control",  "no-cache, no-store, must-revalidate")
      resp.http.setHeader("Pragma",  "no-cache")
      resp.http.setDateHeader("Expires",  0)
    }
    if(cacheFlags.privateCacheMode) {
      resp.http.setHeader("Cache-Control", "private")
    }
  }
}

case class httpOkResult(override val cacheFlags : CachingStrategy= CachingStrategy()) extends ApiMethodResult(cacheFlags)
object httpOkResult extends httpOkResult(CachingStrategy())
case class httpNoResult(override val cacheFlags : CachingStrategy= CachingStrategy()) extends ApiMethodResult(cacheFlags)
object httpNoResult extends httpNoResult(CachingStrategy())

case class httpTextResult(textResult : String, override val cacheFlags : CachingStrategy= CachingStrategy()) extends ApiMethodResult(cacheFlags) {
  override def proceedHttpResponse(resp : HttpResourceResponse) = {
	   super.proceedHttpResponse(resp)
	   resp.http.setContentType("text/plain")
	   resp.http.getWriter().append(textResult)
	   resp.http.getWriter().flush()	   
  }
} 
case class httpJsonResult[T<:AnyRef](jsonObj : T, override val cacheFlags : CachingStrategy= CachingStrategy()) extends ApiMethodResult(cacheFlags) {
  //lazy val textResult = Json.generate(jsonObj)
  lazy val textResult = JSonSerializer.serialize(jsonObj)

  override def proceedHttpResponse(resp : HttpResourceResponse) = {
	  super.proceedHttpResponse(resp)
      resp.http.setContentType("application/json")
      resp.http.getWriter().append(textResult)
      resp.http.getWriter().flush()
  }  
}

case class httpXmlResult(xmlObj : Node, override val cacheFlags : CachingStrategy= CachingStrategy()) extends ApiMethodResult(cacheFlags)  {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
	  super.proceedHttpResponse(resp)
      resp.http.setContentType("text/xml")
      scala.xml.XML.write(resp.http.getWriter(), xmlObj, "utf-8", true, null)
      resp.http.getWriter().flush()
	}  
}
case class httpRedirectResult(url : String, override val cacheFlags : CachingStrategy= CachingStrategy())  extends ApiMethodResult(cacheFlags) {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
		super.proceedHttpResponse(resp)	  
		resp.http.sendRedirect( url )	  
	}
}

case class httpErrorResult(errorCode : Int, errorString : String, override val cacheFlags : CachingStrategy= CachingStrategy())  extends ApiMethodResult(cacheFlags) {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
		super.proceedHttpResponse(resp)
		resp.http.sendError(errorCode, errorString)	  
	}
}

abstract class ApiMethodBodyHandler(val restricted : Boolean)

class ApiMethodBodyHandlerNoParams(restricted : Boolean, apiMethodBody : => ApiMethodResult) extends ApiMethodBodyHandler(restricted) {
  lazy val handler = apiMethodBody
}	
case class ApiMethodBodyHandlerHttpRequest(override val restricted : Boolean, val handler : (HttpResourceRequest)=> ApiMethodResult) extends ApiMethodBodyHandler(restricted)
case class ApiMethodBodyHandlerHttpRequestAndResponse(override val restricted : Boolean, val handler : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) extends ApiMethodBodyHandler(restricted)


trait ApiDsl {

	trait ApiMethod {
	  def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef
	  def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef
	  def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef
	}
	
	case class ApiMethodDef  (
	    restricted : Boolean = false,
	    path : Option[String] = None, 
	    httpMethod : HttpMethod = HttpMethod.ANY_METHOD, 
	    handler : Option[ApiMethodBodyHandler] = None) extends ApiMethod  {

		def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(restricted, path, httpMethod, Option(new ApiMethodBodyHandlerNoParams(restricted,apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(restricted, path, httpMethod, Option(ApiMethodBodyHandlerHttpRequest(restricted, apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(restricted, path, httpMethod, Option(ApiMethodBodyHandlerHttpRequestAndResponse(restricted, apiMethodBody)))
		}
		
		def execute() = {
		  handler match {
		      case Some(body : ApiMethodBodyHandlerNoParams) => body.handler
		      case _ => throw new Exception("Unable to execute empty or wrong request handler!")
		  }
		}
		
		def execute(req : HttpResourceRequest) = {
		  handler match {
		      case Some(body : ApiMethodBodyHandlerNoParams) => body.handler
		      case Some(body : ApiMethodBodyHandlerHttpRequest) => body.handler(req)
		      case _ => throw new Exception("Unable to execute empty or wrong request handler!")
		  }
		}

		def execute(req : HttpResourceRequest, resp : HttpResourceResponse) = {
		  handler match {
		      case Some(body : ApiMethodBodyHandlerNoParams) => body.handler
		      case Some(body : ApiMethodBodyHandlerHttpRequest) => body.handler(req)
		      case Some(body : ApiMethodBodyHandlerHttpRequestAndResponse) => body.handler(req, resp)
		      case _ => throw new Exception("Unable to execute empty or wrong request handler!")
		  }
		}
		
	} 
	
	class ApiMethodImpl(val restricted : Boolean =false) extends ApiMethod {
		def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(restricted, None, HttpMethod.ANY_METHOD, Option(new ApiMethodBodyHandlerNoParams(restricted,apiMethodBody)))
		} 
		def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(restricted, None, HttpMethod.ANY_METHOD, Option(ApiMethodBodyHandlerHttpRequest(restricted,apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(restricted, None, HttpMethod.ANY_METHOD, Option(ApiMethodBodyHandlerHttpRequestAndResponse(restricted,apiMethodBody)))
		}
		
		def apply : ApiMethodDef = {
		  ApiMethodDef(restricted)
		}
	  
		def apply(path : String, httpMethod : HttpMethod = HttpMethod.ANY_METHOD) : ApiMethodDef = {
		  ApiMethodDef(restricted, Option(path), httpMethod)
		}
		
		def apply(httpMethod : HttpMethod) : ApiMethodDef = {
		  ApiMethodDef(restricted, None, httpMethod)
		}			
	}
	
	object apiMethod extends ApiMethodImpl {	  
	}
	
	object restrictedApiMethod extends ApiMethodImpl(true) {	  
	}
	

}
