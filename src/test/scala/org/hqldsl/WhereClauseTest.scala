/*
 * Copyright (c) 2009, Steven Blundy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
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

  test("Greater Than") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" GT Literal(1))
    victim.queryString should equal ("SELECT test FROM test WHERE a > 1")
  }

  test("Greater Than/Equals") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" GE Literal(1))
    victim.queryString should equal ("SELECT test FROM test WHERE a >= 1")
  }

  test("Less Than/Equals") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" LE Literal(1))
    victim.queryString should equal ("SELECT test FROM test WHERE a <= 1")
  }

  test("Less Than") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" LT Literal(1))
    victim.queryString should equal ("SELECT test FROM test WHERE a < 1")
  }

  test("member of") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" MEMBER_OF "allAs")
    victim.queryString should equal ("SELECT test FROM test WHERE a MEMBER OF allAs")
  }

  test("not member of") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" NOT_MEMBER_OF "allAs")
    victim.queryString should equal ("SELECT test FROM test WHERE a NOT MEMBER OF allAs")
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

  test("Is Null") {
    val victim = SELECT("test") FROM ("test" AS "t") WHERE (Prop("t", "name") IS_NULL)
    victim.queryString should equal ("SELECT test FROM test AS t WHERE t.name IS NULL")
  }

  test("Is Not Null") {
    val victim = SELECT("test") FROM ("test" AS "t") WHERE (Prop("t", "name") IS_NOT_NULL)
    victim.queryString should equal ("SELECT test FROM test AS t WHERE t.name IS NOT NULL")
  }

  test("Between") {
    val victim = SELECT("test") FROM ("test" AS "t") WHERE (Prop("t", "name") BETWEEN Literal(0) AND Literal(10))
    victim.queryString should equal ("SELECT test FROM test AS t WHERE t.name BETWEEN 0 AND 10")
  }

  test("Subquery") {
    val victim = SELECT("test") FROM ("test") WHERE
            ("a" IN (SELECT("st") FROM ("subtest" AS "st") WHERE ("b" EQ Literal(4))))
    victim.queryString should equal ("SELECT test FROM test WHERE a IN (SELECT st FROM subtest AS st WHERE b = 4)")
  }

  test("NOT") {
    val victim = SELECT("test") FROM ("test") WHERE NOT ("name" EQ Literal("test"))
    victim.queryString should equal ("SELECT test FROM test WHERE NOT name = 'test'")
  }

  test("NOT AND") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ Literal(1)) AND NOT ("name" EQ Literal("test"))
    victim.queryString should equal ("SELECT test FROM test WHERE a = 1 AND NOT name = 'test'")
  }

  test("Nested NOT") {
    val victim = SELECT("test") FROM ("test") WHERE ("a" EQ Literal(1)) AND (NOT ("name" EQ Literal("test")) OR ("c" NE "d"))
    victim.queryString should equal ("SELECT test FROM test WHERE a = 1 AND (NOT name = 'test' OR c <> d)")
  }
}