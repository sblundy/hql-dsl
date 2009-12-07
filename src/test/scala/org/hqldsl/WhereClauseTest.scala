package org.hqldsl

import org.junit.Assert._
import org.junit.Test
import org.scalatest.Suite

class WhereClauseTest extends Suite with HqlQuerying {
  @Test def testWhere() {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ 1)
    assertEquals("SELECT test FROM test WHERE 1 = 1", victim.queryString)
  }

  @Test def testWhere2() {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ 1) AND (2 NE 1)
    assertEquals("SELECT test FROM test WHERE 1 = 1 AND 2 <> 1", victim.queryString)
  }

  @Test def testWhere3() {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ 1) OR (2 NE 1)
    assertEquals("SELECT test FROM test WHERE 1 = 1 OR 2 <> 1", victim.queryString)
  }

  @Test def testWhere4() {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ "a")
    assertEquals("SELECT test FROM test WHERE 'a' = 'a'", victim.queryString)
  }

  @Test def testVariable() {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ Var(1))
    assertEquals("SELECT test FROM test WHERE 1 = :var", victim.queryString.substring(0, 36))
    assertTrue(victim.variables match {
      case Variable(_, 1) :: Nil => true
      case _ => false
    })
  }

  @Test def testProperty() {
    val victim = SELECT("test") FROM ("test") WHERE (Prop("name") EQ "test")
    assertEquals("SELECT test FROM test WHERE name = 'test'", victim.queryString)
  }

  @Test def testPropertyWithTableAlias() {
    val victim = SELECT("test") FROM ("test" AS "t") WHERE (Prop("t", "name") EQ "test")
    assertEquals("SELECT test FROM test AS t WHERE t.name = 'test'", victim.queryString)
  }
}