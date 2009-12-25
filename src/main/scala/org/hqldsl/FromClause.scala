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

class FromClause(val select:Option[SelectClause], val tables:Seq[Table])
        extends ExecutableClause with WhereProvider with GroupByProvider with HavingProvider
                with OrderByProvider {
  def queryString():String = {
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
  protected[hqldsl] def variables:Seq[Variable[_]] = Nil
}

trait FromImplicits {
  implicit def string2Table(name:String):RootTable with Aliasable = new RootTable(name, None) with AliasableRoot
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