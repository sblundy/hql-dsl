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

class WhereClause(from:ExecutableClause, last:TreeNode)
        extends ExpressionsClause(last) with GroupByProvider with HavingProvider with OrderByProvider {
  type T = WhereClause

  def AND(c:Criterion):WhereClause = new WhereClause(from, LinkedNode(last, Junction.and, c))
  def OR(c:Criterion):WhereClause = new WhereClause(from, LinkedNode(last, Junction.or, c))
  def queryString():String = from.queryString + " WHERE " + walk(last)
}

abstract class ExpressionsClause(last:TreeNode) extends ExecutableClause {
  type T <: ExpressionsClause
  
  def AND(c:Criterion):T
  def OR(c:Criterion):T

  protected def walk(node:TreeNode):String = {
    def printCriterion(c:Criterion):String = {
      import ExpressionClause._
      c match {
        case BinaryCriterion(left, op, right) => BinaryCriterionFormat.format(mkString(left), mkString(op), mkString(right))
        case UnitaryCriterion(left, op) => UnitaryCriterionFormat.format(mkString(left), mkString(op))
        case BetweenCriteron(left, mid, right) => BetweenCriterionFormat.format(mkString(left), mkString(mid), mkString(right))
        case NodeCriterion(node) => NodeCriterionFormat.format(walk(node))
        case SubQueryCriterion(left, op, subquery) => SubQueryCriterionFormat.format(mkString(left), mkString(op), mkString(subquery))
        case NotCriterion(c) => NotCriterionFormat.format(printCriterion(c))
        case EmptyCriterion(op, query) => EmptyCriterionFormat.format(mkString(op), mkString(query))
      }
    }

    node match {
      case FirstNode(c) => printCriterion(c)
      case LinkedNode(previous, j, c) => walk(previous) + " " + mkString(j) + " " + printCriterion(c)
    }
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

  private def mkString(v:Junction):String = v match {
    case Junction.and => "AND"
    case Junction.or => "OR"
  }

  private def mkString(v:Op):String = v match {
    case BinaryOp.eq => "="
    case BinaryOp.ne => "<>"
    case BinaryOp.gt => ">"
    case BinaryOp.ge => ">="
    case BinaryOp.le => "<="
    case BinaryOp.lt => "<"
    case BinaryOp.like => "LIKE"
    case BinaryOp.memberOf => "MEMBER OF"
    case BinaryOp.notMemberOf => "NOT MEMBER OF"
    case UnitaryOp.isNotNull => "IS NOT NULL"
    case UnitaryOp.isNull => "IS NULL"
    case CollectionOp.in => "IN"
    case CollectionOp.notIn => "NOT IN"
    case EmptyOp.isNotEmpty => "IS NOT EMPTY"
    case EmptyOp.isEmpty => "IS EMPTY"
  }

  protected[hqldsl] def variables:Seq[Variable[_]] = {
    criteria.flatMap(_ match {
      case BinaryCriterion(left:Variable[_], _, right:Variable[_]) => List(left, right)
      case BinaryCriterion(_, _, right:Variable[_]) => List(right)
      case BinaryCriterion(left:Variable[_], _, _) => List(left)
      case SubQueryCriterion(_, _, SubQuery(query)) => query.variables
      case EmptyCriterion(_, SubQuery(query)) => query.variables
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

object ExpressionClause {
  import runtime.RichString

  protected[hqldsl] val BinaryCriterionFormat = new RichString("%s %s %s")
  protected[hqldsl] val UnitaryCriterionFormat = new RichString("%s %s")
  protected[hqldsl] val BetweenCriterionFormat = new RichString("%s BETWEEN %s AND %s")
  protected[hqldsl] val NodeCriterionFormat = new RichString("(%s)")
  protected[hqldsl] val SubQueryCriterionFormat = new RichString("%s %s %s")
  protected[hqldsl] val NotCriterionFormat = new RichString("NOT %s")
  protected[hqldsl] val EmptyCriterionFormat = new RichString("%s %s")
}

trait WhereProvider {
  self:ExecutableClause =>
  def WHERE(c:Criterion):WhereClause = new WhereClause(this, FirstNode(c))
}

trait ExpressionsClauseImplicits {
  implicit def atom2Left(x:CriterionAtom):Left = new Left(x)
  implicit def string2Left(x:String):Left = new Left(Prop(x))
  implicit def criterionToTree(node:TreeNode):Criterion = NodeCriterion(node)

  object NOT {
    def apply(c:Criterion):Criterion = NotCriterion(c)
  }
}

class Left(val left:CriterionAtom) {
  def EQ(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.eq, right)
  def EQ(right:String):Criterion = new BinaryCriterion(left, BinaryOp.eq, Prop(right))
  def NE(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.ne, right)
  def NE(right:String):Criterion = new BinaryCriterion(left, BinaryOp.ne, Prop(right))
  def GT(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.gt, right)
  def GT(right:String):Criterion = new BinaryCriterion(left, BinaryOp.gt, Prop(right))
  def GE(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.ge, right)
  def GE(right:String):Criterion = new BinaryCriterion(left, BinaryOp.ge, Prop(right))
  def LE(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.le, right)
  def LE(right:String):Criterion = new BinaryCriterion(left, BinaryOp.le, Prop(right))
  def LT(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.lt, right)
  def LT(right:String):Criterion = new BinaryCriterion(left, BinaryOp.lt, Prop(right))
  def LIKE(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.like, right)
  def LIKE(right:String):Criterion = new BinaryCriterion(left, BinaryOp.like, Prop(right))
  def MEMBER_OF(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.memberOf, right)
  def MEMBER_OF(right:String):Criterion = new BinaryCriterion(left, BinaryOp.memberOf, Prop(right))
  def NOT_MEMBER_OF(right:CriterionAtom):Criterion = new BinaryCriterion(left, BinaryOp.notMemberOf, right)
  def NOT_MEMBER_OF(right:String):Criterion = new BinaryCriterion(left, BinaryOp.notMemberOf, Prop(right))
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

case class BinaryCriterion(left:CriterionAtom, op:BinaryOp, right:CriterionAtom) extends Criterion
case class UnitaryCriterion(atom:CriterionAtom, op:UnitaryOp) extends Criterion
case class BetweenCriteron(left:CriterionAtom, mid:CriterionAtom, right:CriterionAtom) extends Criterion
case class NodeCriterion(tree:TreeNode) extends Criterion
case class SubQueryCriterion(left:CriterionAtom, op:CollectionOp, subquery:SubQuery) extends Criterion
case class NotCriterion(c:Criterion) extends Criterion
case class EmptyCriterion(op:EmptyOp, query:SubQuery) extends Criterion

abstract sealed class TreeNode extends NotNull

case class LinkedNode(previous:TreeNode, j:Junction, criterion:Criterion) extends TreeNode
case class FirstNode(criterion:Criterion) extends TreeNode

sealed trait Op extends NotNull

sealed trait BinaryOp extends Op

object BinaryOp {
  case object eq extends BinaryOp
  case object ne extends BinaryOp
  case object gt extends BinaryOp
  case object ge extends BinaryOp
  case object le extends BinaryOp
  case object lt extends BinaryOp
  case object like extends BinaryOp
  case object memberOf extends BinaryOp
  case object notMemberOf extends BinaryOp
}

sealed trait UnitaryOp extends Op

object UnitaryOp {
  case object isNull extends UnitaryOp
  case object isNotNull extends UnitaryOp
}

sealed trait EmptyOp extends Op

object EmptyOp {
  case object isEmpty extends EmptyOp
  case object isNotEmpty extends EmptyOp
}

sealed trait CollectionOp extends Op

object CollectionOp {
  case object in extends CollectionOp
  case object notIn extends CollectionOp
}

sealed trait Junction extends NotNull

object Junction {
  case object and extends Junction
  case object or extends Junction
}

object IS_EMPTY {
  def apply(query:ExecutableClause) = EmptyCriterion(EmptyOp.isEmpty, SubQuery(query))
}

object IS_NOT_EMPTY {
  def apply(query:ExecutableClause) = EmptyCriterion(EmptyOp.isNotEmpty, SubQuery(query))
}