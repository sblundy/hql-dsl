package org.hqldsl

class WhereClause(from:FromClause, last:TreeNode) extends Clause {
  def AND(c:Criterion):WhereClause = new WhereClause(from, LinkedNode(last, Junction.and, c))
  def OR(c:Criterion):WhereClause = new WhereClause(from, LinkedNode(last, Junction.or, c))
  override def queryString():String = {
    def walk(node:TreeNode):String = node match {
      case FirstNode(Criterion(left, op, right)) => mkString(left) + " " + mkString(op) + " " + mkString(right)
      case LinkedNode(previous, j, Criterion(left, op, right)) => walk(previous) + " " +
        mkString(j) + " " + mkString(left) + " " + mkString(op) + " " + mkString(right)
    }
    from.queryString + " WHERE " + walk(last)
  }

  private def mkString(v:Any):String = v match {
    case s:String => "'" + s + "'"
    case Variable(name, _) => ":" + name
    case Property(null, name, null) => name
    case Property(obj, name, null) => obj + "." + name
    case Junction.and => "AND"
    case Junction.or => "OR"
    case Op.eq => "="
    case Op.ne => "<>"
    case primative:AnyVal => primative.toString
    case d:java.util.Date => throw new IllegalArgumentException("dates not supported")
    case a:AnyRef => throw new IllegalArgumentException("type not supported:" + a.getClass.getName)
  }

  private[hqldsl] def variables:Seq[Variable[_]] = {
    criteria.flatMap(_ match {
      case Criterion(left:Variable[_], _, right:Variable[_]) => List(left, right)
      case Criterion(_, _, right:Variable[_]) => List(right)
      case Criterion(left:Variable[_], _, _) => List(left)
      case _ => Nil
    })
  }

  private def criteria:Seq[Criterion] = {
    def walk(node:TreeNode):List[Criterion] = node match {
      case FirstNode(c) => List(c)
      case LinkedNode(previous, _, c) => walk(previous) ++ List(c)
    }
    walk(this.last)
  }
}

class Left[L](left:L) {
  def EQ[R](right:R):Criterion = new Criterion(left, Op.eq, right)
  def NE[R](right:R):Criterion = new Criterion(left, Op.ne, right)
}

case class Variable[T](name:String, value:T)
case class Property(obj:String, name:String, alias:String)

object Var {
  private val r = new java.util.Random()
  def apply[T](value:T):Variable[T] = Variable[T]("var" + r.nextInt(Integer.MAX_VALUE), value)
}

object Prop {
  def apply(name:String):Property = Property(null, name, null)
  def apply(obj:String, name:String):Property = Property(obj, name, null)
}

case class Criterion(left:Any, op:Op.Value, right:Any)

abstract sealed class TreeNode extends NotNull

case class LinkedNode(previous:TreeNode, j:Junction.Value, criterion:Criterion) extends TreeNode
case class FirstNode(criterion:Criterion) extends TreeNode

object Op extends Enumeration with NotNull {
  val eq = Value
  val ne = Value
}

object Junction extends Enumeration with NotNull {
  val and = Value
  val or = Value
}