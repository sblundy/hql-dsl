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