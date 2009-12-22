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

class WhereClause(from:FromClause, last:TreeNode) extends ExecutableClause {
  def AND(c:Criterion):WhereClause = new WhereClause(from, LinkedNode(last, Junction.and, c))
  def OR(c:Criterion):WhereClause = new WhereClause(from, LinkedNode(last, Junction.or, c))
  override def queryString():String = {
    def walk(node:TreeNode):String = {
      def printCriterion(c:Criterion):String = {
        c match {
          case BinaryCriterion(left, op, right) => mkString(left) + " " + mkString(op) + " " + mkString(right)
          case UnitaryCriterion(left, op) => mkString(left) + " " + mkString(op)
          case BetweenCriteron(left, mid, right) =>
            mkString(left) + " BETWEEN " + mkString(mid) + " AND " +mkString(right)
          case NodeCriterion(node) => "(" + walk(node) + ")"
          case SubQueryCriterion(left, op, subquery) => mkString(left) + " " + mkString(op) + " " + mkString(subquery)
          case NotCriterion(c) => "NOT " + printCriterion(c)
        }
      }

      node match {
        case FirstNode(c) => printCriterion(c)
        case LinkedNode(previous, j, c) => walk(previous) + " " + mkString(j) + " " + printCriterion(c)
      }
    }
    from.queryString + " WHERE " + walk(last)
  }

  private def mkString(v:CriterionAtom):String = v match {
    case Variable(name, _) => ":" + name
    case Property(None, name) => name
    case Property(Some(obj), name) => obj + "." + name
    case Literal(s:String) => "'" + s + "'"
    case Literal(primative:AnyVal) => primative.toString
    case Literal(d:java.util.Date) => throw new IllegalArgumentException("dates not supported for literals")
    case Literal(a:AnyRef) =>
      throw new IllegalArgumentException("type not supported for literals:" + a.getClass.getName)
    case SubQuery(subquery) => "(" + subquery.queryString + ")"
  }

  private def mkString(v:CollectionOp):String = v match {
    case CollectionOp.in => "IN"
    case CollectionOp.notIn => "NOT IN"
  }

  private def mkString(v:Junction):String = v match {
    case Junction.and => "AND"
    case Junction.or => "OR"
  }

  private def mkString(v:Op):String = v match {
    case Op.eq => "="
    case Op.ne => "<>"
    case Op.gt => ">"
    case Op.ge => ">="
    case Op.le => "<="
    case Op.lt => "<"
    case Op.like => "LIKE"
  }

  private def mkString(v:UnitaryOp):String = v match {
    case UnitaryOp.isNotNull => "IS NOT NULL"
    case UnitaryOp.isNull => "IS NULL"
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

  object NOT {
    def apply(c:Criterion):Criterion = NotCriterion(c)
  }
}

class Left(val left:CriterionAtom) {
  def EQ(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.eq, right)
  def EQ(right:String):Criterion = new BinaryCriterion(left, Op.eq, Prop(right))
  def NE(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.ne, right)
  def NE(right:String):Criterion = new BinaryCriterion(left, Op.ne, Prop(right))
  def GT(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.gt, right)
  def GT(right:String):Criterion = new BinaryCriterion(left, Op.gt, Prop(right))
  def GE(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.ge, right)
  def GE(right:String):Criterion = new BinaryCriterion(left, Op.ge, Prop(right))
  def LE(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.le, right)
  def LE(right:String):Criterion = new BinaryCriterion(left, Op.le, Prop(right))
  def LT(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.lt, right)
  def LT(right:String):Criterion = new BinaryCriterion(left, Op.lt, Prop(right))
  def LIKE(right:CriterionAtom):Criterion = new BinaryCriterion(left, Op.like, right)
  def LIKE(right:String):Criterion = new BinaryCriterion(left, Op.like, Prop(right))
  def IS_NULL:Criterion = new UnitaryCriterion(left, UnitaryOp.isNull)
  def IS_NOT_NULL:Criterion = new UnitaryCriterion(left, UnitaryOp.isNotNull)
  def BETWEEN(one:CriterionAtom):BetweenTemp = new BetweenTemp(left, one)
  def IN(right:ExecutableClause):Criterion = new SubQueryCriterion(left, CollectionOp.in, new SubQuery(right))
  def NOT_IN(right:ExecutableClause):Criterion = new SubQueryCriterion(left, CollectionOp.notIn, new SubQuery(right))
}

class BetweenTemp(left:CriterionAtom, mid:CriterionAtom) {
  def AND(right:CriterionAtom):Criterion = new BetweenCriteron(left, mid, right)
}
sealed trait CriterionAtom

case class Variable[T](name:String, value:T) extends CriterionAtom
case class Literal[T](value:T) extends CriterionAtom
case class Property(obj:Option[String], name:String) extends CriterionAtom
case class SubQuery(subquery:ExecutableClause) extends CriterionAtom

object Var {
  private val r = new java.util.Random()
  def apply[T](value:T):Variable[T] = Variable[T]("var" + r.nextInt(Integer.MAX_VALUE), value)
}

object Prop {
  def apply(name:String):Property = Property(None, name)
  def apply(obj:String, name:String):Property = Property(Some(obj), name)
}

sealed trait Criterion extends NotNull {
  def AND(c:Criterion):TreeNode = LinkedNode(FirstNode(this), Junction.and, c)
  def OR(c:Criterion):TreeNode = LinkedNode(FirstNode(this), Junction.or, c)
}

case class BinaryCriterion(left:CriterionAtom, op:Op, right:CriterionAtom) extends Criterion
case class UnitaryCriterion(atom:CriterionAtom, op:UnitaryOp) extends Criterion
case class BetweenCriteron(left:CriterionAtom, mid:CriterionAtom, right:CriterionAtom) extends Criterion
case class NodeCriterion(tree:TreeNode) extends Criterion
case class SubQueryCriterion(left:CriterionAtom, op:CollectionOp, subquery:SubQuery) extends Criterion
case class NotCriterion(c:Criterion) extends Criterion

abstract sealed class TreeNode extends NotNull

case class LinkedNode(previous:TreeNode, j:Junction, criterion:Criterion) extends TreeNode
case class FirstNode(criterion:Criterion) extends TreeNode

sealed trait Op extends NotNull

object Op {
  case object eq extends Op
  case object ne extends Op
  case object gt extends Op
  case object ge extends Op
  case object le extends Op
  case object lt extends Op
  case object like extends Op
}

sealed trait UnitaryOp extends NotNull

object UnitaryOp {
  case object isNull extends UnitaryOp
  case object isNotNull extends UnitaryOp
}

sealed trait CollectionOp extends NotNull

object CollectionOp {
  case object in extends CollectionOp
  case object notIn extends CollectionOp
}

sealed trait Junction extends NotNull

object Junction {
  case object and extends Junction
  case object or extends Junction
}