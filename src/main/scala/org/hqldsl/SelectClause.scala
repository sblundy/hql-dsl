package org.hqldsl

class SelectClause(val projections:String*) extends Clause {
  def FROM(tables:Table*):FromClause = new FromClause(Some(this), tables)
  override def queryString():String = "SELECT " + projections.mkString(", ")
}