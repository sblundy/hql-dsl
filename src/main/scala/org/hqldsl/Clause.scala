package org.hqldsl

abstract class Clause {
  def queryString():String
}

abstract class ExecutableClause extends Clause {
  protected[hqldsl] def variables:Seq[Variable[_]]
}