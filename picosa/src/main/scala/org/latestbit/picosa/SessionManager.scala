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

package org.latestbit.picosa

import org.apache.commons.codec.binary.Hex
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import javax.crypto.KeyGenerator
import java.net.URLEncoder
import java.net.URLDecoder

case class SessionParams(userId : String, authParams : String, timestamp : Long)

class SessionManager(val algorithm : String = "HmacSHA1") { 	

	def createSessionKey( key : String, sessionValue : String ) : String = {
	  createSessionKey(Hex.decodeHex(key.toCharArray()), sessionValue)
	}
	
	def createSessionKey( key : String, userId : String, authParams : String, timestamp : Long ) : String = {
	  createSessionKey(Hex.decodeHex(key.toCharArray()), userId, authParams, timestamp)
	}
	
	def createSessionKey( key : Array[Byte], sessionValue : String ) : String = {
	   
	  val keySpec = new SecretKeySpec(
        key, algorithm 
      )
	  val encrypt = Mac.getInstance(algorithm)
	  encrypt.init(keySpec)
	  sessionValue+":"+Hex.encodeHexString(encrypt.doFinal( sessionValue.getBytes("UTF-8") ))	  
	}
	  
	def createSessionKey( key : Array[Byte], userId : String, authParams : String, timestamp : Long ) : String = {	   
	  createSessionKey(key, formatSessionParams(userId, authParams, timestamp))  
	}
	
	def formatSessionParams(userId : String, authParams : String, timestamp : Long) = 
	  userId+":"+URLEncoder.encode(authParams, "UTF-8")+":"+timestamp			
	
	def decodeSessionParams(key : String, sessionKey : String) : SessionParams = {
	  decodeSessionParams(Hex.decodeHex(key.toCharArray()),sessionKey)
	}
	def decodeSessionParams(key : Array[Byte], sessionKey : String) : SessionParams = {
	  val decodedStr = sessionKey.split(":")
	  val decodedParams = SessionParams(decodedStr(0), URLDecoder.decode(decodedStr(1), "UTF-8"), decodedStr(2).toLong)
	  val checkSessionValidity = createSessionKey(key, decodedParams.userId, decodedParams.authParams, decodedParams.timestamp)
	  if(checkSessionValidity.equals(sessionKey)) {
	    decodedParams
	  }
	  else
	    null
	}
	
	def decodeSessionValue(key : String, sessionValue : String) : String = {
	  decodeSessionValue(Hex.decodeHex(key.toCharArray()),sessionValue)
	}
	
	def decodeSessionValue(key : Array[Byte], sessionValue : String) : String = {
	  val decodedStr = sessionValue.split(":")
	  val checkSessionValidity = createSessionKey(key, decodedStr(0))
	  if(checkSessionValidity.equals(sessionValue)) {
	    decodedStr(0)
	  }
	  else
	    null
	}
	
	def generateKey() : Array[Byte] = {
	  KeyGenerator.getInstance(algorithm).generateKey().getEncoded()
	}
	
	def generateKeyAsString() : String = {
	  keyToString(generateKey())
	}
	
	def keyToString(key : Array[Byte]) : String = {
	  Hex.encodeHexString(key)
	}
}