package org.ml4ai.mdp

import org.sarsamora.states.State

// TODO: Keep adding fields to the constructor
case class WikiHopState(iterationNum:Int,
                        numNodes:Int,
                        numEdges:Int,
                        startEntity:Seq[String],
                        endEntity:Seq[String],
                        candidateEntities:Option[Seq[Seq[String]]] // Store the candidate entities in the state for future usage
                       ) extends State {
  override def toFeatures: Map[String, Double] =
    Map(
      "iterationNum" -> iterationNum,
      "numNodes" -> numNodes,
      "numEdges" -> numEdges,
    )
}

object WikiHopState{
  val numFeatures:Int = 3
}
