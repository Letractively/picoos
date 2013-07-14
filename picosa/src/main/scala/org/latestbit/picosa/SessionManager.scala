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


class SessionManager {
  
	val algorithm = "HmacSHA1"
	  
	def createSessionKey( key : String, userId : String, authType : String, expiration : Long ) : String = {
	  createSessionKey(Hex.decodeHex(key.toCharArray()), userId, authType, expiration)
	}
	  
	def createSessionKey( key : Array[Byte], userId : String, authType : String, expiration : Long ) : String = {
	   
	  val keySpec = new SecretKeySpec(
        key, algorithm 
      )
	  val encrypt = Mac.getInstance(algorithm)
	  encrypt.init(keySpec)
	  Hex.encodeHexString(encrypt.doFinal( (userId+":"+authType+":"+expiration.toString).getBytes() ))	  
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