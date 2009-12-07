package org.hqldsl

import org.junit.Assert._
import org.junit.Test
import org.scalatest.Suite


class FromClauseTest extends Suite with HqlQuerying {
  @Test def testSimple() {
    val victim = SELECT("test") FROM "test"
    assertEquals("SELECT test FROM test", victim.queryString)
  }

  @Test def testSimple2() {
    val victim = SELECT("test") FROM ("test", "test2")
    assertEquals("SELECT test FROM test, test2", victim.queryString)
  }

  @Test def testAlias() {
    val victim = SELECT("test") FROM ("test" AS "t")
    assertEquals("SELECT test FROM test AS t", victim.queryString)
  }

  @Test def testAlias2() {
    val victim = SELECT("test") FROM ("test" AS "t", "test2" AS "t2")
    assertEquals("SELECT test FROM test AS t, test2 AS t2", victim.queryString)
  }
}