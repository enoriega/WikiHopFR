package org.ml4ai.learning

import org.sarsamora.actions.Action

case class EpisodeStats(id:String, iterations:Int, papersRead:Int, success:Boolean, actions:Seq[Action]) {
  override def toString: String = {
    val actionsString = actions.map(_.toString).mkString("\t")
    s"$id\t$iterations\t$papersRead\t$success\t$actionsString"
  }
}
