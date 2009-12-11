package org.hqldsl

import org.hibernate.Session
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class WhereClauseTest extends FunSuite with HqlQuerying with SessionSource with ShouldMatchers {
  def session() = null.asInstanceOf[Session]

  test("Where") {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ 1)
    victim.queryString should equal ("SELECT test FROM test WHERE 1 = 1")
  }

  test("Where2") {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ 1) AND (2 NE 1)
    victim.queryString should equal ("SELECT test FROM test WHERE 1 = 1 AND 2 <> 1")
  }

  test("Where3") {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ 1) OR (2 NE 1)
    victim.queryString should equal ("SELECT test FROM test WHERE 1 = 1 OR 2 <> 1")
  }

  test("Where4") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ "a")
    victim.queryString should equal ("SELECT test FROM test WHERE 'a' = 'a'")
  }

  test("Nested") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ "a") AND (("b" EQ "b") OR ("c" EQ "c"))
    victim.queryString should equal ("SELECT test FROM test WHERE 'a' = 'a' AND ('b' = 'b' OR 'c' = 'c')")
  }

  test("Nested 2") {
    val victim = SELECT("test") FROM ("test") WHERE (("b" EQ "b") AND ("c" EQ "c")) OR ("a" EQ "a")
    victim.queryString should equal ("SELECT test FROM test WHERE ('b' = 'b' AND 'c' = 'c') OR 'a' = 'a'")
  }

  test("Nested Deep") {
    val victim = SELECT("test") FROM ("test") WHERE
            (("b" EQ "b") AND ("c" EQ "c")) OR ((("a" EQ "a") OR ("d" NE "f")) AND ("e" EQ "e"))
    victim.queryString should equal (
      "SELECT test FROM test WHERE ('b' = 'b' AND 'c' = 'c') OR (('a' = 'a' OR 'd' <> 'f') AND 'e' = 'e')")
  }

  test("Variable") {
    val victim = SELECT("test") FROM ("test") WHERE (1 EQ Var(1))
    victim.queryString.substring(0, 36) should equal ("SELECT test FROM test WHERE 1 = :var")
    victim.variables.size should equal(1)
    victim.variables.first.name should fullyMatch regex("var\\d+")
    victim.variables.first.value should equal(1)
  }

  test("Property") {
    val victim = SELECT("test") FROM ("test") WHERE (Prop("name") EQ "test")
    victim.queryString should equal ("SELECT test FROM test WHERE name = 'test'")
  }

  test("PropertyWithTableAlias") {
    val victim = SELECT("test") FROM ("test" AS "t") WHERE (Prop("t", "name") EQ "test")
    victim.queryString should equal ("SELECT test FROM test AS t WHERE t.name = 'test'")
  }
}