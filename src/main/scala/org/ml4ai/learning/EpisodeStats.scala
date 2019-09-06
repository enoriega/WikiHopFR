package org.ml4ai.learning

import org.sarsamora.actions.Action

case class EpisodeStats(id:String, iterations:Int, papersRead:Int, actions:Seq[Action])
