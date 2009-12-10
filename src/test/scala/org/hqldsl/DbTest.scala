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
            SELECT("s") FROM (classOf[Simple].getName AS "s") WHERE (Prop("s.name") EQ "Name 1")

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
            SELECT("s") FROM (classOf[Simple].getName AS "s") WHERE (Prop("s.name") NE "Name 1") AND (Prop("s.name") NE "Name 2")

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
            SELECT("c") FROM (classOf[Container].getName AS "c" JOIN "c.simples" AS "s") WHERE (Prop("s.name") EQ "Name 1")

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
            SELECT("c") FROM (classOf[Container].getName AS "c" JOIN "c.simples") WHERE (Prop("name") EQ "Name 1")

          values.size should be(2)
        }
      }
    }
  }
}