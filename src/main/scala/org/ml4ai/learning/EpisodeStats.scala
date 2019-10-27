package org.ml4ai.learning

import org.sarsamora.actions.Action

case class EpisodeStats(id:String, iterations:Int, papersRead:Int,
                        success:Boolean, epsilons:Seq[Double],
                        rewards:Seq[Double], actions:Seq[Action],
                        numEntities:Int
                       ) {
  override def toString: String = {
    val actionsString = (epsilons, rewards, actions).zipped.map{
      case (epsilon, reward, action) =>
        s"$epsilon/$reward/$action"
    }.mkString("\t")
    s"$id\t$iterations\t$papersRead\t$success\t$actionsString"
  }
}
