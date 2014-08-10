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

package org.latestbit.picoos.serializers

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.core.`type`.TypeReference
import java.io.StringWriter
import java.lang.reflect._
import java.util.logging.Logger
import java.io.InputStream


object JSonSerializer {
    private final val log : Logger  = Logger.getLogger(JSonSerializer.getClass().getName())
    
	val mapper = new ObjectMapper()
	mapper.registerModule(DefaultScalaModule)
	
	def serialize(value: Any): String = {
		val writer = new StringWriter()
		mapper.writeValue(writer, value)
		writer.toString
	}
	
	def deserialize[T: Manifest](value: String) : T =  {
	  try {
		  mapper.readValue(value, typeReference[T])
	  }
	  catch{
	    case (ex:com.fasterxml.jackson.core.JsonParseException) => {
	      log.warning(s"JSon deserialize error $ex")
	      null.asInstanceOf[T]
	    }
	    case (ex:com.fasterxml.jackson.databind.JsonMappingException) => {
	      log.warning(s"JSon mapping error $ex for json: \n$value\n ")
	      throw ex;
	    }
	  }
	}


	def deserialize[T: Manifest](stream: InputStream) : T =  {
	  try {
		  mapper.readValue(stream, typeReference[T])
	  }
	  catch{
	    case (ex:com.fasterxml.jackson.core.JsonParseException) => {
	      log.warning(s"JSon deserialize error $ex")
	      null.asInstanceOf[T]
	    }
	    case (ex:com.fasterxml.jackson.databind.JsonMappingException) => {
	      log.warning(s"JSon mapping error $ex for json stream\n ")
	      throw ex;
	    }
	  }
	}
	
	def deserialize[T <: AnyRef](value: String, cls : Class[T]) : T =  {
	  mapper.readValue(value, cls)
	}
	
	def deserializeToObj[T](value: String, obj: T)(implicit m: scala.Predef.Manifest[T]): T = {
	  try {
	  	mapper.reader(m.runtimeClass.asInstanceOf[Class[T]]).withValueToUpdate(obj).readValue(value.getBytes())
	  }
	  catch{
	    case (ex:com.fasterxml.jackson.core.JsonParseException) => {
	      log.warning(s"JSon deserialize error $ex")
	      null.asInstanceOf[T]
	    }
	    case (ex:com.fasterxml.jackson.databind.JsonMappingException) => {
	      log.warning(s"JSon mapping error $ex for json: \n$value\n ")
	      throw ex;
	    }
	  }
    } 
	
	private [this] def typeReference[T: Manifest] = new TypeReference[T] {
	  override def getType = typeFromManifest(manifest[T])
	}
	
	private [this] def typeFromManifest(m: Manifest[_]): Type = {
	  if (m.typeArguments.isEmpty) { m.runtimeClass }
	  else new ParameterizedType {
	    def getRawType = m.runtimeClass
	    def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray
	    def getOwnerType = null
	  }
	}
	
}
