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

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class FromClauseTest extends HqlQueriesTestBase with FunSuite with ShouldMatchers {
  test("Simple") {
    val victim = SELECT("test") FROM "test"
    victim.queryString should equal ("SELECT test FROM test")
  }

  test("No Select") {
    val victim = FROM("test")
    victim.queryString should equal ("FROM test")
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