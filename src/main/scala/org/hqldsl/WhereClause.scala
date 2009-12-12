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
    case Literal(s:String) => "'" + s + "'"
    case Variable(name, _) => ":" + name
    case Property(None, name, _) => name
    case Property(Some(obj), name, _) => obj + "." + name
    case Junction.and => "AND"
    case Junction.or => "OR"
    case Op.eq => "="
    case Op.ne => "<>"
    case Literal(primative:AnyVal) => primative.toString
    case Literal(d:java.util.Date) => throw new IllegalArgumentException("dates not supported")
    case Literal(a:AnyRef) => throw new IllegalArgumentException("type not supported:" + a.getClass.getName)
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
  implicit def atom2Left(x:CriterionAtom):Left = new Left(x)
  implicit def string2Left(x:String):Left = new Left(Prop(x))
  implicit def criterionToTree(node:TreeNode):Criterion = NodeCriterion(node)
}

class Left(left:CriterionAtom) {
  def EQ(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.eq, right)
  def EQ(right:String):Criterion = new BinaryCriterion(left, Op.eq, Prop(right))
  def NE(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.ne, right)
  def NE(right:String):Criterion = new BinaryCriterion(left, Op.ne, Prop(right))
}

sealed trait CriterionAtom

case class Variable[T](name:String, value:T) extends CriterionAtom
case class Literal[T](value:T) extends CriterionAtom
case class Property(obj:Option[String], name:String, alias:Option[String]) extends CriterionAtom

object Var {
  private val r = new java.util.Random()
  def apply[T](value:T):Variable[T] = Variable[T]("var" + r.nextInt(Integer.MAX_VALUE), value)
}

object Prop {
  def apply(name:String):Property = Property(None, name, None)
  def apply(obj:String, name:String):Property = Property(Some(obj), name, None)
}

sealed trait Criterion extends NotNull {
  def AND(c:Criterion):TreeNode = LinkedNode(FirstNode(this), Junction.and, c)
  def OR(c:Criterion):TreeNode = LinkedNode(FirstNode(this), Junction.or, c)
}

case class BinaryCriterion(left:CriterionAtom, op:Op.Value, right:CriterionAtom) extends Criterion
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