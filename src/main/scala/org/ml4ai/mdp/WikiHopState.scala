package org.ml4ai.mdp

import org.sarsamora.states.State

// TODO: Keep adding fields to the constructor
case class WikiHopState(iterationNum:Int,
                        numNodes:Int,
                        numEdges:Int,
                        startEntity:Set[String],
                        endEntity:Set[String],
                        candidateEntities:Option[Seq[Set[String]]], // Store the candidate entities in the state for future usage
                        iterationsOfIntroduction:Seq[Int],
                        ranks:Seq[Int],
                        entityUsage:Seq[Int],
                        pairwiseComponents:Map[(Set[String], Set[String]), Boolean]
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
