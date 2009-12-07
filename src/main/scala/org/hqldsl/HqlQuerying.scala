package org.hqldsl

trait HqlQuerying {
  def SELECT(projections:String*):SelectClause = new SelectClause(projections:_*)

  implicit def any2Left[L](x:L):Left[L] = new Left(x)
  implicit def string2Table(name:String):Table with Aliasable = new Table(name, null) with Aliasable
}