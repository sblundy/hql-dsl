package org.hqldsl

import org.hibernate.Session
import collection.jcl.{Buffer, Conversions}

trait HqlQuerying extends FromImplicits with WhereImplicits {
  this: SessionSource =>
  def SELECT(projections:String*):SelectClause = new SelectClause(projections:_*)
  def FROM(tables:Table*):FromClause = new FromClause(None, tables)

  implicit def exec[T](executable:ExecutableClause):Buffer[T] = {
    val query = session.createQuery(executable.queryString)

    executable.variables.foreach(_ match {
      case Variable(name, value) => query.setParameter(name, value)
    })

    return Conversions.convertList(query.list.asInstanceOf[java.util.List[T]])
  }
}

trait SessionSource {
  protected def session():Session
}