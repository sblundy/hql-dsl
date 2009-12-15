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
import collection.jcl.Buffer
import org.easymock.{IMocksControl, EasyMock}
import org.hibernate.{Query, Session}
import java.util.Arrays
import org.scalatest.matchers.ShouldMatchers

class ExecTest extends FunSuite with ShouldMatchers {
  def withSession(testFunction:(IMocksControl, Session) => Unit) {
    val ctrl = EasyMock.createControl
    val sess = ctrl.createMock(classOf[Session])
    testFunction(ctrl, sess)
  }

  test("Simple Exec") {
    withSession {
      (ctrl, sess) => {
        new HqlQuerying with SessionSource {
          def session = sess

          val query = ctrl.createMock(classOf[Query])
          EasyMock.expect(sess.createQuery(EasyMock.anyObject[String])).andReturn(query)
          EasyMock.expect(query.list.asInstanceOf[java.util.List[Integer]]).andReturn(Arrays.asList(Integer.MAX_VALUE))

          ctrl.replay
          val results:Buffer[AnyRef] = SELECT("test") FROM "test" WHERE ("name" EQ Literal("test"))
          ctrl.verify

          results should have size (1)
          results.contains(Integer.MAX_VALUE) should be (true)
        }
      }
    }
  }
}