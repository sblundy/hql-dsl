package org.hqldsl.testmodel

import javax.persistence._

@Entity
class Container {
  @GeneratedValue{ val strategy=GenerationType.IDENTITY }
  @Id
  var id:Long = _

  @Basic
  var containerName:String = _

  @ManyToMany
  var simples:java.util.Set[Simple] = _
}