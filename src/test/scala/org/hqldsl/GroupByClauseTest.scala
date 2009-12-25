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

class GroupByClauseTest extends HqlQueriesTestBase with FunSuite with ShouldMatchers {
  test("Default") {
    val victim = SELECT("count(*)") FROM "test" GROUP_BY "test"
    victim.queryString should equal ("SELECT count(*) FROM test GROUP BY test")
  }

  test("Multiple") {
    val victim = SELECT("test", "count(name)") FROM "test" GROUP_BY ("test", "name")
    victim.queryString should equal ("SELECT test, count(name) FROM test GROUP BY test, name")
  }

  test("w/ WHERE") {
    val victim = SELECT("test", "count(name)") FROM "test" WHERE ("x" EQ Var(4)) GROUP_BY ("test", "name")
    victim.queryString should fullyMatch regex ("SELECT test, count\\(name\\) FROM test WHERE x = :var\\d+ GROUP BY test, name")
    victim.variables.size should be (1)
  }
}