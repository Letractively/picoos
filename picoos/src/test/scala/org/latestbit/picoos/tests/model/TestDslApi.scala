package org.latestbit.picoos.tests.model

import org.latestbit.picoos._
import org.latestbit.picoos.dsl._


class TestDsl extends ApiDsl {
  def test0 = requireAuth restMethod as {
    httpNoResult
  }
  
  def test1 = restMethod as {
    httpNoResult
  }
  
  def test2 = restMethod( path = "ggg" ) as {
    httpNoResult
  }
  
  
  def test3 = requireAuth restMethod as {
    httpNoResult
  }
  
  def test4 = requireAuth restMethod( httpMethod = HttpMethod.GET ) as {
    httpNoResult
  }
  
  def test5 = requireAuth(permissions = Seq("Str", "Str2")) restMethod( path = "ggghhjhj", httpMethod = HttpMethod.GET ) as {
    httpNoResult
  }
  
  def test6 = requireAuth(permissions = Seq("Str", "Str2"), authFunction=None) restMethod( path = "ggghhjhj", httpMethod = HttpMethod.GET ) as {
    httpNoResult
  }

}