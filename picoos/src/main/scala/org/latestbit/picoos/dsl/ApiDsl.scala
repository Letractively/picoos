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


case class CachingOptions(val noCacheMode : Boolean = false, val privateCacheMode : Boolean = true)

abstract class ApiMethodResult(val cacheFlags : CachingOptions) {
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

case class httpOkResult(override val cacheFlags : CachingOptions= CachingOptions()) extends ApiMethodResult(cacheFlags)
object httpOkResult extends httpOkResult(CachingOptions())
case class httpNoResult(override val cacheFlags : CachingOptions= CachingOptions()) extends ApiMethodResult(cacheFlags)
object httpNoResult extends httpNoResult(CachingOptions())

case class httpTextResult(textResult : String, override val cacheFlags : CachingOptions= CachingOptions()) extends ApiMethodResult(cacheFlags) {
  override def proceedHttpResponse(resp : HttpResourceResponse) = {
	   super.proceedHttpResponse(resp)
	   resp.http.setContentType("text/plain")
	   resp.http.getWriter().append(textResult)
	   resp.http.getWriter().flush()	   
  }
} 
case class httpJsonResult[T<:AnyRef](jsonObj : T, override val cacheFlags : CachingOptions= CachingOptions()) extends ApiMethodResult(cacheFlags) {
  //lazy val textResult = Json.generate(jsonObj)
  lazy val textResult = JSonSerializer.serialize(jsonObj)

  override def proceedHttpResponse(resp : HttpResourceResponse) = {
	  super.proceedHttpResponse(resp)
      resp.http.setContentType("application/json")
      resp.http.getWriter().append(textResult)
      resp.http.getWriter().flush()
  }  
}

case class httpXmlResult(xmlObj : Node, override val cacheFlags : CachingOptions= CachingOptions()) extends ApiMethodResult(cacheFlags)  {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
	  super.proceedHttpResponse(resp)
      resp.http.setContentType("text/xml")
      scala.xml.XML.write(resp.http.getWriter(), xmlObj, "utf-8", true, null)
      resp.http.getWriter().flush()
	}  
}
case class httpRedirectResult(url : String, override val cacheFlags : CachingOptions= CachingOptions())  extends ApiMethodResult(cacheFlags) {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
		super.proceedHttpResponse(resp)	  
		resp.http.sendRedirect( url )	  
	}
}

case class httpErrorResult(errorCode : Int, errorString : String, override val cacheFlags : CachingOptions= CachingOptions())  extends ApiMethodResult(cacheFlags) {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
		super.proceedHttpResponse(resp)
		resp.http.sendError(errorCode, errorString)	  
	}
}

abstract class ApiMethodBodyHandler(val properties : Map[String, AnyVal])

class ApiMethodBodyHandlerNoParams(properties : Map[String, AnyVal], apiMethodBody : => ApiMethodResult) extends ApiMethodBodyHandler(properties) {
  lazy val handler = apiMethodBody
}	
class ApiMethodBodyHandlerHttpRequest(properties : Map[String, AnyVal], val handler : (HttpResourceRequest)=> ApiMethodResult) extends ApiMethodBodyHandler(properties)
class ApiMethodBodyHandlerHttpRequestAndResponse(properties : Map[String, AnyVal], val handler : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) extends ApiMethodBodyHandler(properties)

object ApiMethodBodyProperties {
  val PROTECTED = "protected"
}

trait ApiDsl {

	trait ApiMethod {
	  def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef
	  def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef
	  def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef
	}
	
	case class ApiMethodDef  (
	    properties : Map[String, AnyVal] = Map(), 
	    path : Option[String] = None, 
	    httpMethod : HttpMethod = HttpMethod.ANY_METHOD, 
	    handler : Option[ApiMethodBodyHandler] = None) extends ApiMethod  {

		def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef( properties, path, httpMethod, Option(new ApiMethodBodyHandlerNoParams(properties, apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(properties, path, httpMethod, Option(new ApiMethodBodyHandlerHttpRequest(properties, apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(properties, path, httpMethod, Option(new ApiMethodBodyHandlerHttpRequestAndResponse(properties, apiMethodBody)))
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
	
	class ApiMethodImpl(val properties : Map[String, AnyVal] = Map()) extends ApiMethod {
		def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(properties, None, HttpMethod.ANY_METHOD, Option(new ApiMethodBodyHandlerNoParams(properties,apiMethodBody)))
		} 
		def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(properties, None, HttpMethod.ANY_METHOD, Option(new ApiMethodBodyHandlerHttpRequest(properties,apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(properties, None, HttpMethod.ANY_METHOD, Option(new ApiMethodBodyHandlerHttpRequestAndResponse(properties,apiMethodBody)))
		}
		
		def apply : ApiMethodDef = {
		  ApiMethodDef(properties)
		}

		def apply(path : String) : ApiMethodDef = {
		  ApiMethodDef(properties, Option(path))
		}
		
		def apply(path : String, httpMethod : HttpMethod) : ApiMethodDef = {
		  ApiMethodDef(properties, Option(path), httpMethod)
		}
		
		def apply(httpMethod : HttpMethod) : ApiMethodDef = {
		  ApiMethodDef(properties, None, httpMethod)
		}	
		
	}
	
	object apiMethod extends ApiMethodImpl {	  
	}
	
	class ProtectedApiMethod extends ApiMethodImpl(Map( ApiMethodBodyProperties.PROTECTED -> true )) {	  
	}
	
	object protectedApiMethod extends ProtectedApiMethod {
	}

}
