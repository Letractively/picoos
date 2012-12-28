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
import com.codahale.jerkson._
import scala.xml.Node

abstract class ApiMethodResult {
  def proceedHttpResponse(resp : HttpResourceResponse) = {}
}

case class httpOkResult() extends ApiMethodResult
object httpOkResult extends httpOkResult
case class httpNoResult() extends ApiMethodResult
object httpNoResult extends httpNoResult

case class httpTextResult(textResult : String) extends ApiMethodResult {
  override def proceedHttpResponse(resp : HttpResourceResponse) = {
	   resp.body.setContentType("text/plain")
	   resp.body.getWriter().append(textResult)
	   resp.body.getWriter().flush()    
  }
} 
case class httpJsonResult[T<:AnyRef](jsonObj : T) extends ApiMethodResult {
  lazy val textResult = Json.generate(jsonObj)

  override def proceedHttpResponse(resp : HttpResourceResponse) = {
      resp.body.setContentType("application/json")
      resp.body.getWriter().append(textResult)
      resp.body.getWriter().flush()
  }
  
}

case class httpXmlResult(xmlObj : Node) extends ApiMethodResult  {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
      resp.body.setContentType("text/xml")
      scala.xml.XML.write(resp.body.getWriter(), xmlObj, "utf-8", true, null)
      resp.body.getWriter().flush()
	}  
}
case class httpRedirectResult(url : String)  extends ApiMethodResult {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
		resp.body.sendRedirect( url )	  
	}
}

case class httpErrorResult(errorCode : Int, errorString : String)  extends ApiMethodResult {
	override def proceedHttpResponse(resp : HttpResourceResponse) = {
		resp.body.sendError(errorCode, errorString)	  
	}
}


abstract class ApiMethodBodyHandler

class ApiMethodBodyHandlerNoParams(apiMethodBody : => ApiMethodResult) extends ApiMethodBodyHandler {
  lazy val handler = apiMethodBody
}	
case class ApiMethodBodyHandlerHttpRequest(val handler : (HttpResourceRequest)=> ApiMethodResult) extends ApiMethodBodyHandler
case class ApiMethodBodyHandlerHttpRequestAndResponse(val handler : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) extends ApiMethodBodyHandler


trait ApiDsl {

	trait ApiMethod {
	  def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef
	  def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef
	  def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef
	}
	
	case class ApiMethodDef  (
	    path : Option[String] = None, 
	    httpMethod : HttpMethod = HttpMethod.ANY_METHOD, 
	    handler : Option[ApiMethodBodyHandler] = None) extends ApiMethod  {

		def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(path, httpMethod, Option(new ApiMethodBodyHandlerNoParams(apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(path, httpMethod, Option(ApiMethodBodyHandlerHttpRequest(apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(path, httpMethod, Option(ApiMethodBodyHandlerHttpRequestAndResponse(apiMethodBody)))
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
	
	
	object apiMethod extends ApiMethod {	  

		def as( apiMethodBody : => ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(None, HttpMethod.ANY_METHOD, Option(new ApiMethodBodyHandlerNoParams(apiMethodBody)))
		} 
		def as( apiMethodBody : (HttpResourceRequest)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(None, HttpMethod.ANY_METHOD, Option(ApiMethodBodyHandlerHttpRequest(apiMethodBody)))
		}
		def as( apiMethodBody : (HttpResourceRequest, HttpResourceResponse)=> ApiMethodResult) : ApiMethodDef = {
		  ApiMethodDef(None, HttpMethod.ANY_METHOD, Option(ApiMethodBodyHandlerHttpRequestAndResponse(apiMethodBody)))
		}
		
		def apply : ApiMethodDef = {
		  ApiMethodDef()
		}
	  
		def apply(path : String, httpMethod : HttpMethod = HttpMethod.ANY_METHOD) : ApiMethodDef = {
		  ApiMethodDef(Option(path), httpMethod)
		}
		
		def apply(httpMethod : HttpMethod) : ApiMethodDef = {
		  ApiMethodDef(None, httpMethod)
		}
		
	}
}
