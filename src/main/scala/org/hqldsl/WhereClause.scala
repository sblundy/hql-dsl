package org.hqldsl

class WhereClause(from:FromClause, last:Criterion[_, _]) extends Clause {
  def AND(c:Criterion[_, _]):WhereClause = new WhereClause(from, Criterion(last, Junction.and, c.left, c.op, c.right))
  def OR(c:Criterion[_, _]):WhereClause = new WhereClause(from, Criterion(last, Junction.or, c.left, c.op, c.right))
  override def queryString():String = {
    def p(c:Criterion[_, _]):String = c match {
      case Criterion(null, null, left, op, right) => mkString(left) + " " + mkString(op) + " " + mkString(right)
      case Criterion(previous, j, left, op, right) =>
        p(previous) + " " + mkString(j) + " " + mkString(left) + " " + mkString(op) + " " + mkString(right)
    }
    from.queryString + " WHERE " + p(last)
  }

  private def mkString(v:Any):String = v match {
    case s:String => "'" + s + "'"
    case Variable(name, _) => ":" + name
    case Junction.and => "AND"
    case Junction.or => "OR"
    case Op.eq => "="
    case Op.ne => "<>"
    case primative:AnyVal => primative.toString
    case d:java.util.Date => throw new IllegalArgumentException("dates not supported")
    case a:AnyRef => throw new IllegalArgumentException("type not supported:" + a.getClass.getName)
  }
}

case class Variable[T](name:String, value:T)

object Var {
  private val r = new java.util.Random()
  def apply[T](value:T):Variable[T] = Variable[T]("var" + r.nextInt, value)
}

case class Criterion[L, R](previous:Criterion[_, _], j:Junction.Value, left:L, op:Op.Value, right:R)

object Op extends Enumeration {
  val eq = Value
  val ne = Value

  implicit def opValue2String(op:Op.Value):String = op match {
    case Op.eq => "="
    case Op.ne => "<>"
  }
}

object Junction extends Enumeration {
  val and = Value
  val or = Value

  implicit def junctionValue2String(j:Junction.Value):String = j match {
    case Junction.and => "AND"
    case Junction.or => "OR"
  }
}