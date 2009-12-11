package org.hqldsl

class FromClause(val select:Option[SelectClause], val tables:Seq[Table]) extends Clause with NotNull {
  def WHERE(c:Criterion):WhereClause = new WhereClause(this, FirstNode(c))
  override def queryString():String = {
    def printTable(t:Table):String = {
      def printAlias(a:Option[String]):String = a match { case None => "" ; case Some(alias) => " AS " + alias }
      
      t match {
        case RootTable(name, alias) => name + printAlias(alias)
        case InnerJoinedTable(target, name, alias) => printTable(target) + " INNER JOIN " + name + printAlias(alias)
        case LeftJoinedTable(target, name, alias) =>  printTable(target) + " LEFT OUTER JOIN " + name + printAlias(alias)
        case RightJoinedTable(target, name, alias) => printTable(target) + " RIGHT OUTER JOIN " + name + printAlias(alias)
        case CrossJoinedTable(target, name, alias) => printTable(target) + " FULL JOIN " + name + printAlias(alias)
      }
    }

    {select match {case None => ""; case Some(s) => s.queryString + " "}} + "FROM " + tables.map(printTable(_)).mkString(", ")
  }
}

sealed trait Table extends NotNull {
  def JOIN(joined:RootTable):InnerJoinedTable with Aliasable =
    new InnerJoinedTable(this, joined.name, joined.alias) with AliasableInnerJoinedTable
  def LEFT(joined:RootTable):LeftJoinedTable with Aliasable =
    new LeftJoinedTable(this, joined.name, joined.alias) with AliasableLeftJoinedTable
  def RIGHT(joined:RootTable):RightJoinedTable with Aliasable =
    new RightJoinedTable(this, joined.name, joined.alias) with AliasableRightJoinedTable
  def FULL(joined:RootTable):CrossJoinedTable with Aliasable =
    new CrossJoinedTable(this, joined.name, joined.alias) with AliasableCrossJoinedTable
}

sealed trait Aliasable {
  type T <: Table
  def AS(alias:String):Table
}

case class RootTable(name:String, alias:Option[String]) extends Table
case class InnerJoinedTable(target:Table, name:String, alias:Option[String]) extends Table
case class LeftJoinedTable(target:Table, name:String, alias:Option[String]) extends Table
case class RightJoinedTable(target:Table, name:String, alias:Option[String]) extends Table
case class CrossJoinedTable(target:Table, name:String, alias:Option[String]) extends Table

trait AliasableRoot extends Aliasable {
  self: RootTable =>
  type T = RootTable
  def AS(alias:String):RootTable = new RootTable(this.name, Some(alias))
}

trait AliasableInnerJoinedTable extends Aliasable {
  self: InnerJoinedTable =>
  type T = InnerJoinedTable
  def AS(alias:String):InnerJoinedTable = new InnerJoinedTable(this.target, this.name, Some(alias))
}

trait AliasableLeftJoinedTable extends Aliasable {
  self: LeftJoinedTable =>
  type T = LeftJoinedTable
  def AS(alias:String):LeftJoinedTable = new LeftJoinedTable(this.target, this.name, Some(alias))
}

trait AliasableRightJoinedTable extends Aliasable {
  self: RightJoinedTable =>
  type T = RightJoinedTable
  def AS(alias:String):RightJoinedTable = new RightJoinedTable(this.target, this.name, Some(alias))
}

trait AliasableCrossJoinedTable extends Aliasable {
  self: CrossJoinedTable =>
  type T = CrossJoinedTable
  def AS(alias:String):CrossJoinedTable = new CrossJoinedTable(this.target, this.name, Some(alias))
}