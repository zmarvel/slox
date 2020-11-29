package com.zackmarvel.slox

import scala.collection.mutable

class Environment {
  private val mutBindings = mutable.Map[String, Any]()
  private val immutBindings = mutable.Map[String, Any]()

  def bindMutable(name: String, value: Any): Unit = {
    mutBindings(name) = value
  }

  def bindImmutable(name: String, value: Any): Unit = {
    immutBindings(name) = value
  }

  def isMutable(name: String): Option[Any] = {
    mutBindings.get(name)
  }

  def setMutable(name: String, value: Any): Unit = {
    // TODO -- error if not declared?
    mutBindings(name) = value
  }
}
