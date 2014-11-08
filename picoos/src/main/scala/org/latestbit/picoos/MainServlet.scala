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

package org.latestbit.picoos


import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import javax.servlet.ServletException
import java.util.logging.Logger
import java.util.logging.Level
import org.latestbit.picoos.HttpMethod._

class MainServlet extends HttpServlet {      
  private final val log : Logger  = Logger.getLogger(classOf[MainServlet].getName())
  
  val DEFAULT_API_CONTEXT = "/rest-api"
    
  override def doGet(req : HttpServletRequest , resp : HttpServletResponse) = {
    proceedRequest(HttpMethod.GET,req,resp)
  }
  
  override def doPut(req : HttpServletRequest , resp : HttpServletResponse) = {
    proceedRequest(HttpMethod.PUT,req,resp)
  }  

  override def doPost(req : HttpServletRequest , resp : HttpServletResponse) = {
    proceedRequest(HttpMethod.POST,req,resp)
  }  

  override def doDelete(req : HttpServletRequest , resp : HttpServletResponse) = {
	  proceedRequest(HttpMethod.DELETE,req,resp)
  }
  
  override def doTrace(req : HttpServletRequest , resp : HttpServletResponse) = {
	  proceedRequest(HttpMethod.TRACE,req,resp)
  }
  
  override def doHead(req : HttpServletRequest , resp : HttpServletResponse) = {
	  proceedRequest(HttpMethod.HEAD,req,resp)
  }
  
  override def doOptions(req : HttpServletRequest , resp : HttpServletResponse) = {
	  proceedRequest(HttpMethod.OPTIONS,req,resp)
  }  
  
  def getAPIPathPrefix()  : String = {
    Option(getServletConfig().getInitParameter("api-prefix")) match {
      case Some(str : String) => str.trim() 
      case _ => DEFAULT_API_CONTEXT
    }
  }
  
  var registry : HttpResourcesRegistry = null
  
  override def init() = {
    super.init()

    registry = Option(getServletConfig().getInitParameter("registry-class")) match {
      case Some(registryClassName: String) => {
    	val registryClass = Class.forName(registryClassName.trim())
    	registryClass.newInstance().asInstanceOf[HttpResourcesRegistry]        
      }
      case _ => new StdHttpResourcesRegistry("Default Registry for {"+getAPIPathPrefix+"}")      
    }
    
    val resourcesFactory : String = getServletConfig().getInitParameter("resources-factory-classes")
    
    if(resourcesFactory==null)
      throw new ServletException("Required 'resources-factory-classes' servlet parameter doesn't not found in web.xml!")
    
    resourcesFactory.split(",").foreach(resourcesFactoryClassName => {
    	val resourcesFactoryClass = Class.forName(resourcesFactoryClassName.trim())
    	val resourcesFactoryInstance = resourcesFactoryClass.newInstance().asInstanceOf[HttpResourcesFactory]
    	resourcesFactoryInstance.createResources(registry, this.getServletContext(), this.getServletConfig())
    })

  }
     
  def proceedRequest(httpMethod : HttpMethod, req : HttpServletRequest , resp : HttpServletResponse) = {
    	val requestUri : String =  Option(req.getAttribute("javax.servlet.include.request_uri")) match {
    	  case Some(vl) => vl.asInstanceOf[String] 
    	  case _ => req.getRequestURI()
    	}
    	
	    val apiPathPrefix = getAPIPathPrefix()
	    val idx = requestUri.indexOf(apiPathPrefix)
	    if(idx != -1) {
	    	val fullPath = requestUri.substring(idx)
	    	val servicePath = fullPath.replaceFirst(
	    	    apiPathPrefix, 
	    	    apiPathPrefix match {
	    	      case "/" => "/"
	    	      case _ => ""
	    	    }
	    	)
	    	val resourceReq = HttpResourceRequest(	    			
		            req,
		            httpMethod,
		            servicePath,
		            apiPathPrefix, 
		            getServletConfig(),
		            getServletContext()
		    )
		    
		    PerfUtils.measureTime {
	    		val resourceResp = HttpResourceResponse(resp)
	    		registry.proceedRequest( resourceReq, resourceResp )
	    	}(time => log.logp(Level.FINE, classOf[MainServlet].getName(), "proceedRequest", s"The request to $servicePath (Method: $httpMethod) has been processed within $time ms."))
	    }
	    else {
	      log.log(Level.WARNING,"Unable to process request to empty or unknow prefix ("+requestUri+"). Check your web.xml settings!")
	      resp.sendError(404, "Unable to process request to empty or unknow prefix ("+requestUri+")")
	    }
   }
}
