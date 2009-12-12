package org.hqldsl

import org.hibernate.Session
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class WhereClauseTest extends FunSuite with HqlQuerying with SessionSource with ShouldMatchers {
  def session() = null.asInstanceOf[Session]

  test("Where") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ Literal(1))
    victim.queryString should equal ("SELECT test FROM test WHERE a = 1")
  }

  test("Where2") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ Literal(1)) AND ("b" NE Literal(1))
    victim.queryString should equal ("SELECT test FROM test WHERE a = 1 AND b <> 1")
  }

  test("Where3") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ Literal(1)) OR ("b" NE Literal(1))
    victim.queryString should equal ("SELECT test FROM test WHERE a = 1 OR b <> 1")
  }

  test("Where4") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ Literal("a"))
    victim.queryString should equal ("SELECT test FROM test WHERE a = 'a'")
  }

  test("Nested") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ "b") AND (("b" EQ "c") OR ("c" EQ "d"))
    victim.queryString should equal ("SELECT test FROM test WHERE a = b AND (b = c OR c = d)")
  }

  test("Nested 2") {
    val victim = SELECT("test") FROM ("test") WHERE (("b" EQ "b") AND ("c" EQ "c")) OR ("a" EQ "a")
    victim.queryString should equal ("SELECT test FROM test WHERE (b = b AND c = c) OR a = a")
  }

  test("Nested Deep") {
    val victim = SELECT("test") FROM ("test") WHERE
            (("b" EQ "b") AND ("c" EQ "c")) OR ((("a" EQ "a") OR ("d" NE "f")) AND ("e" EQ "e"))
    victim.queryString should equal (
      "SELECT test FROM test WHERE (b = b AND c = c) OR ((a = a OR d <> f) AND e = e)")
  }

  test("Variable") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ Var(1))
    victim.queryString.substring(0, 36) should equal ("SELECT test FROM test WHERE a = :var")
    victim.variables.size should equal(1)
    victim.variables.first.name should fullyMatch regex("var\\d+")
    victim.variables.first.value should equal(1)
  }

  test("Property") {
    val victim = SELECT("test") FROM ("test") WHERE ("name" EQ "test")
    victim.queryString should equal ("SELECT test FROM test WHERE name = test")
  }

  test("PropertyWithTableAlias") {
    val victim = SELECT("test") FROM ("test" AS "t") WHERE (Prop("t", "name") EQ Literal("test"))
    victim.queryString should equal ("SELECT test FROM test AS t WHERE t.name = 'test'")
  }
}