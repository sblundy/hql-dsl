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
import org.hibernate.cfg.AnnotationConfiguration
import java.util.Date
import collection.jcl.Buffer
import org.slf4j.{LoggerFactory, Logger}
import org.hibernate.{Session, SessionFactory}
import testmodel.{Container, Simple}

class DbTest extends FunSuite with ShouldMatchers {
  private val log:Logger = LoggerFactory.getLogger(classOf[DbTest])
  private val sf:SessionFactory = new AnnotationConfiguration().configure().buildSessionFactory()

  private def init() {
    xaction {
      (sess) => {
        val one = new Simple
        one.name = "Name 1"
        one.reference = new Date
        sess.save(one)
        val two = new Simple
        two.name = "Name 2"
        two.reference = new Date
        sess.save(two)

        val c1 = new Container
        c1.simples = new java.util.HashSet[Simple]
        c1.simples.add(one)
        c1.simples.add(two)
        sess.save(c1)
        val c2 = new Container
        c2.simples = new java.util.HashSet[Simple]
        c2.simples.add(one)
        sess.save(c2)
        val c3 = new Container
        c3.simples = new java.util.HashSet[Simple]
        sess.save(c3)
      }
    }
  }

  init

  private def xaction(testFunction: (Session) => Unit) {
    val sess = sf.openSession

    try {
      sess.beginTransaction

      testFunction(sess)

      sess.getTransaction.commit
    } catch {
      case t => {
        sess.getTransaction.rollback
        throw t
      }
    } finally {
      sess.close
    }
  }

  protected abstract class TestHqlQuerying(val session:Session) extends HqlQuerying with SessionSource

  test("Simple") {
    xaction {
      (sess) => {
        new TestHqlQuerying(sess) {
          val values:Buffer[Simple] =
            SELECT("s") FROM (classOf[Simple].getName AS "s") WHERE (Prop("s", "name") EQ Literal("Name 1"))

          values.size should be(1)
          values.first.name should equal("Name 1")
        }
      }
    }
  }

  test("Simple:empty") {
    xaction {
      (sess) => {
        new TestHqlQuerying(sess) {
          val values:Buffer[Simple] =
            SELECT("s") FROM (classOf[Simple].getName AS "s") WHERE
                    (Prop("s", "name") NE Literal("Name 1")) AND (Prop("s", "name") NE Literal("Name 2"))

          values.size should be(0)
        }
      }
    }
  }

  test("Complex w/ alias") {
    xaction {
      (sess) => {
        new TestHqlQuerying(sess) {
          val values:Buffer[Container] =
            SELECT("c") FROM (classOf[Container].getName AS "c" JOIN "c.simples" AS "s") WHERE
                    (Prop("s", "name") EQ Literal("Name 1"))

          values.size should be(2)
        }
      }
    }
  }

  test("Complex") {
    xaction {
      (sess) => {
        new TestHqlQuerying(sess) {
          val values:Buffer[Container] =
            SELECT("c") FROM (classOf[Container].getName AS "c" JOIN "c.simples") WHERE ("name" EQ Literal("Name 1"))

          values.size should be(2)
        }
      }
    }
  }
}