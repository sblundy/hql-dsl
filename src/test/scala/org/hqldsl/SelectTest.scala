package org.hqldsl

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

import org.hqldsl.Predef._

class SelectTest extends AssertionsForJUnit {
  @Test def simple() {
    val victim = SELECT("test") FROM "test"
    assertEquals("SELECT test FROM test", victim.toString)
  }

  @Test def simple2() {
    val victim = SELECT("test") FROM ("test", "test2")
    assertEquals("SELECT test FROM test, test2", victim.toString)
  }
  
  @Test def alias() {
    val victim = SELECT("test") FROM ("test" AS "t")
    assertEquals("SELECT test FROM test AS t", victim.toString)
  }

  @Test def alias2() {
    val victim = SELECT("test") FROM ("test" AS "t", "test2" AS "t2")
    assertEquals("SELECT test FROM test AS t, test2 AS t2", victim.toString)
  }
}