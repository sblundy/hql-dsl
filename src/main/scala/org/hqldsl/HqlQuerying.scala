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

import org.hibernate.Session
import collection.jcl.Buffer

/**
 * Interface to provide HQL queries. Typically a user class will make a query as a part of an
 * internal method. For example, a method that finds a class by name and birthday might look like
 * this:
 *
 * <code>
 * def findByNameAndBirthDateRange(name:String, start:Date, end:Date):Seq[Person] = {
 *   return SELECT "p" FROM "Persons" AS "p" WHERE
 *     "p.name" EQ Var(name) AND "p.birthday" BETWEEN Var(start) AND Var(end)
 * }
 * </code>
 *
 * The {@link Var} objects will correctly handle the variables, passing them to the query via
 * <code>setParameter()</code>.
 */
trait HqlQuerying extends FromImplicits with WhereImplicits with OrderByImplicits {
  this: SessionSource =>
  protected def SELECT(projections:String*):SelectClause = new SelectClause(projections:_*)
  protected def FROM(tables:Table*):FromClause = new FromClause(None, tables)

  implicit protected def exec[T](executable:ExecutableClause):Buffer[T] = {
    val query = session.createQuery(executable.queryString)

    executable.variables.foreach(_ match {
      case Variable(name, value) => query.setParameter(name, value)
    })

    return collection.jcl.Conversions.convertList(query.list.asInstanceOf[java.util.List[T]])
  }
}

/**
 * Used to connect {@link HqlQuerying} to the user class. 
 */
trait SessionSource {
  protected def session():Session
}