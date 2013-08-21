package org.latestbit.picoos

object PerfUtils {
  def meausureTime[T](body : => T)(resultFunc : Double => Unit) : T = {
    val startTime = System.nanoTime()
    val result = body
    resultFunc( (System.nanoTime()-startTime )/1e6 )
    result
  }
}