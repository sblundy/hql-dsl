package org.hqldsl

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

import org.hqldsl.Predef._

class SelectTest extends AssertionsForJUnit {
  @Test def simple() {
    val victim = SELECT("test") FROM "test"
    assertEquals("SELECT test FROM test", victim.queryString)
  }

  @Test def simple2() {
    val victim = SELECT("test") FROM ("test", "test2")
    assertEquals("SELECT test FROM test, test2", victim.queryString)
  }
  
  @Test def alias() {
    val victim = SELECT("test") FROM ("test" AS "t")
    assertEquals("SELECT test FROM test AS t", victim.queryString)
  }

  @Test def alias2() {
    val victim = SELECT("test") FROM ("test" AS "t", "test2" AS "t2")
    assertEquals("SELECT test FROM test AS t, test2 AS t2", victim.queryString)
  }

  @Test def where() {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ 1)
    assertEquals("SELECT test FROM test WHERE 1 = 1", victim.queryString)
  }

  @Test def where2() {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ 1) AND (2 NE 1)
    assertEquals("SELECT test FROM test WHERE 1 = 1 AND 2 <> 1", victim.queryString)
  }

  @Test def where3() {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ "a")
    assertEquals("SELECT test FROM test WHERE 'a' = 'a'", victim.queryString)
  }

  @Test def variable() {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ Var(1))
    assertEquals("SELECT test FROM test WHERE 1 = :var", victim.queryString.substring(0, 36))
  }
}