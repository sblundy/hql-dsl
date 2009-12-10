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

  test("Join") {
    val victim = SELECT("test") FROM ("test" JOIN "subs")
    victim.queryString should equal ("SELECT test FROM test INNER JOIN subs")
  }

  test("Chained Join") {
    val victim = SELECT("test") FROM ("test" AS "t" JOIN "t.subs" AS "s" LEFT "s.subsubs" AS "ss")
    victim.queryString should equal ("SELECT test FROM test AS t INNER JOIN t.subs AS s LEFT OUTER JOIN s.subsubs AS ss")
  }

  test("Inner Join w/ Alias") {
    val victim = SELECT("test") FROM ("test" AS "t" JOIN "t.subs" AS "s")
    victim.queryString should equal ("SELECT test FROM test AS t INNER JOIN t.subs AS s")
  }

  test("Left Join w/ Alias") {
    val victim = SELECT("test") FROM ("test" AS "t" LEFT "t.subs" AS "s")
    victim.queryString should equal ("SELECT test FROM test AS t LEFT OUTER JOIN t.subs AS s")
  }

  test("Right Join w/ Alias") {
    val victim = SELECT("test") FROM ("test" AS "t" RIGHT "t.subs" AS "s")
    victim.queryString should equal ("SELECT test FROM test AS t RIGHT OUTER JOIN t.subs AS s")
  }

  test("Cross Join w/ Alias") {
    val victim = SELECT("test") FROM ("test" AS "t" FULL "t.subs" AS "s")
    victim.queryString should equal ("SELECT test FROM test AS t FULL JOIN t.subs AS s")
  }
}