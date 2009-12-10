package org.hqldsl.testmodel

import java.util.Date
import javax.persistence._

@Entity
class Simple {
  @GeneratedValue{ val strategy=GenerationType.IDENTITY }
  @Id
  var id:Long = _

  @Basic
  var name:String = _

  @Basic
  var reference:Date = _
}