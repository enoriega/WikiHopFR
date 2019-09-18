package org.ml4ai.mdp

import util.hashing.MurmurHash3
import org.sarsamora.actions.Action

case class Exploration(entityHash:Seq[String]) extends Action

case class ExplorationDouble(entityHashA:Seq[String], entityHashB:Seq[String]) extends Action {

  // These methods make sure this action is commutable for its arguments
  override def canEqual(that: Any): Boolean = that.isInstanceOf[ExplorationDouble]

  override def equals(obj: Any): Boolean = obj match {
    case that:ExplorationDouble => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode(): Int = MurmurHash3.unorderedHash(Iterable(entityHashA, entityHashB))

}

case class Exploitation(entityHashA:Seq[String], entityHashB:Seq[String]) extends Action

case object RandomAction extends Action

// This class is meant to be use only for the baseline analysis
case class Cascade(entityHashA:Seq[String], entityHashB:Seq[String]) extends Action
