package org.ml4ai.learning

import org.sarsamora.actions.Action

case class EpisodeStats(id:String, iterations:Int, papersRead:Int, success:Boolean, epsilons:Seq[Double], actions:Seq[Action]) {
  override def toString: String = {
    val actionsString = (epsilons zip actions).map{
      case (epsilon, action) =>
        s"$epsilon/$action"
    }.mkString("\t")
    s"$id\t$iterations\t$papersRead\t$success\t$actionsString"
  }
}
