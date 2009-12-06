package org.hqldsl

class FromClause(val select:SelectClause, val tables:Seq[Table]) extends Clause {
  def WHERE(t:Criterion[_, _]):WhereClause = new WhereClause(this, t)
  override def queryString():String = select.queryString + " FROM " + tables.map(
    _ match {
      case Table(name, null) => name
      case Table(name, alias) => name + " AS " + alias
    }
    ).mkString(", ")
}

trait Aliasable {
  self: Table =>
  def AS(alias:String):Table = new Table(this.name, alias)
}

case class Table(name:String, alias:String)