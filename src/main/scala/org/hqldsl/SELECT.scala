package org.hqldsl

object SELECT {
  def apply(projections:String*):SelectClause = {
    return new SelectClause(projections:_*)
  }
}

abstract class Clause {
  def queryString():String
}

object Predef {
  class Left[L](left:L) {
    def EQ[R](right:R):Criterion[L, R] = new Criterion(null, null, left, Op.eq, right)
    def NE[R](right:R):Criterion[L, R] = new Criterion(null, null, left, Op.ne, right)
  }

  implicit def any2Left[L](x:L):Left[L] = new Left(x)
  implicit def string2Table(name:String):Table with Aliasable = new Table(name, null) with Aliasable
}