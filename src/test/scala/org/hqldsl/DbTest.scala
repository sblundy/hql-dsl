package org.hqldsl

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.hibernate.cfg.AnnotationConfiguration
import java.util.Date
import testmodel.Simple
import collection.jcl.Buffer
import org.slf4j.{LoggerFactory, Logger}
import org.hibernate.{Session, SessionFactory}

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
            SELECT("s") FROM (classOf[Simple].getName AS "s") WHERE (Prop("s.name") NE "Name 1")

          values.size should be(0)
        }
      }
    }
  }
}