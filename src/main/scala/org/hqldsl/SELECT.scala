package org.hqldsl

object SELECT {
  def apply(projections:String*):SelectClause = {
    return new SelectClause(projections:_*)
  }
}

class SelectClause(val projections:String*) {
  def FROM(tables:Table*):FromClause = new FromClause(this, tables)
  override def toString():String = "SELECT " + projections.mkString(", ")
}

class FromClause(val select:SelectClause, val tables:Seq[Table]) {
  def WHERE(t:Criterion[_, _]):WhereClause = new WhereClause(this, t)
  override def toString():String = select + " FROM " + tables.map(
    _ match {
      case Table(name, null) => name
      case Table(name, alias) => name + " AS " + alias
    }
    ).mkString(", ")
}

class WhereClause(from:FromClause, last:Criterion[_, _]) {
  def AND(c:Criterion[_, _]):WhereClause = new WhereClause(from, Criterion(last, Junction.and, c.left, c.op, c.right))
  def OR(c:Criterion[_, _]):WhereClause = new WhereClause(from, Criterion(last, Junction.or, c.left, c.op, c.right))
  override def toString():String = {
    def p(c:Criterion[_, _]):String = {
      if (null == c.previous) {
        c.left + " " + (c.op match {
          case Op.eq => "="
          case Op.ne => "<>"
        }) + " " + c.right
      } else {
        p(c.previous) + " " + (c.j match {
          case Junction.and => "AND"
          case Junction.or => "OR"
        }) + " " + c.left + " " + c.op + " " + c.right
      }
    }
    from.toString + " WHERE " + p(last)
  }
}

trait Aliasable {
  self: Table =>
  def AS(alias:String):Table = new Table(this.name, alias)
}

case class Table(name:String, alias:String)
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

object Predef {
  class Left[L](left:L) {
    def EQ[R](right:R):Criterion[L, R] = new Criterion(null, null, left, Op.eq, right)
    def NE[R](right:R):Criterion[L, R] = new Criterion(null, null, left, Op.ne, right)
  }
  implicit def any2Left[L](x:L):Left[L] = new Left(x)
  implicit def string2Table(name:String):Table with Aliasable = new Table(name, null) with Aliasable
}