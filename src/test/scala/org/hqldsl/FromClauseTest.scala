package org.hqldsl

import org.hibernate.Session
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class FromClauseTest extends FunSuite with HqlQuerying with SessionSource with ShouldMatchers {
  def session() = null.asInstanceOf[Session]

  test("Simple") {
    val victim = SELECT("test") FROM "test"
    victim.queryString should equal ("SELECT test FROM test")
  }

  test("Simple2") {
    val victim = SELECT("test") FROM ("test", "test2")
    victim.queryString should equal ("SELECT test FROM test, test2")
  }

  test("Alias") {
    val victim = SELECT("test") FROM ("test" AS "t")
    victim.queryString should equal ("SELECT test FROM test AS t")
  }

  test("Alias2") {
    val victim = SELECT("test") FROM ("test" AS "t", "test2" AS "t2")
    victim.queryString should equal ("SELECT test FROM test AS t, test2 AS t2")
  }
}