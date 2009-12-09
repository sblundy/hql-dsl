package org.hqldsl

import org.hibernate.Session
import collection.jcl.Buffer

trait HqlQuerying {
  this: SessionSource =>
  def SELECT(projections:String*):SelectClause = new SelectClause(projections:_*)

  implicit def any2Left[L](x:L):Left[L] = new Left(x)
  implicit def string2Table(name:String):Table with Aliasable = new Table(name, null) with Aliasable
  implicit def exec[T](query:WhereClause):Buffer[T] = {
    val q = session.createQuery(query.queryString)

    query.variables.foreach(_ match {
      case Variable(name, value) => q.setParameter(name, value)
    })

    return scala.collection.jcl.Conversions.convertList(q.list.asInstanceOf[java.util.List[T]])
  }
}

trait SessionSource {
  protected def session():Session
}