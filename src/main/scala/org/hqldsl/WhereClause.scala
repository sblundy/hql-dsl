package org.hqldsl

class WhereClause(from:FromClause, last:TreeNode) extends ExecutableClause {
  def AND(c:Criterion):WhereClause = new WhereClause(from, LinkedNode(last, Junction.and, c))
  def OR(c:Criterion):WhereClause = new WhereClause(from, LinkedNode(last, Junction.or, c))
  override def queryString():String = {
    def walk(node:TreeNode):String = {
      def printCriterion(c:Criterion):String = {
        c match {
          case BinaryCriterion(left, op, right) => mkString(left) + " " + mkString(op) + " " + mkString(right)
          case NodeCriterion(node) => "(" + walk(node) + ")"
        }
      }

      node match {
        case FirstNode(c) => printCriterion(c)
        case LinkedNode(previous, j, c) => walk(previous) + " " + mkString(j) + " " + printCriterion(c)
      }
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

  protected[hqldsl] def variables:Seq[Variable[_]] = {
    criteria.flatMap(_ match {
      case BinaryCriterion(left:Variable[_], _, right:Variable[_]) => List(left, right)
      case BinaryCriterion(_, _, right:Variable[_]) => List(right)
      case BinaryCriterion(left:Variable[_], _, _) => List(left)
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

trait WhereImplicits {
  implicit def any2Left[L](x:L):Left[L] = new Left(x)
  implicit def criterionToTree(node:TreeNode):Criterion = NodeCriterion(node)
}

class Left[L](left:L) {
  def EQ[R](right:R):Criterion = new BinaryCriterion(left, Op.eq, right)
  def NE[R](right:R):Criterion = new BinaryCriterion(left, Op.ne, right)
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

sealed trait Criterion extends NotNull {
  def AND(c:Criterion):TreeNode = LinkedNode(FirstNode(this), Junction.and, c)
  def OR(c:Criterion):TreeNode = LinkedNode(FirstNode(this), Junction.or, c)
}

case class BinaryCriterion(left:Any, op:Op.Value, right:Any) extends Criterion
case class NodeCriterion(tree:TreeNode) extends Criterion

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