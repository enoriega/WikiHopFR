package org.ml4ai.mdp

import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.sarsamora.states.State

// TODO: Keep adding fields to the constructor
case class WikiHopState(iterationNum:Int,
                        numNodes:Int,
                        numEdges:Int,
                        startEntity:Set[String],
                        endEntity:Set[String],
                        candidateEntities:Option[Seq[Set[String]]], // Store the candidate entities in the state for future usage
                        candidateEntitiesTypes:Option[Seq[Set[String]]],
                        candidateEntitiesOrigins:Option[Seq[Set[EntityOrigin]]],
                        iterationsOfIntroduction:Seq[Int],
                        ranks:Seq[Int],
                        entityUsage:Seq[Int],
                        pairwiseComponents:Seq[Boolean],
                        successful:Boolean,
                        exploreScores:Seq[Float],
                        exploitScores:Seq[Float],
                       ) extends State {
  override def toFeatures: Map[String, Double] =
    Map(
      "iterationNum" -> iterationNum,
      "numNodes" -> numNodes,
      "numEdges" -> numEdges,
      "successful" -> (if(successful) 1.0 else 0.0),
    )
}

object WikiHopState {
  implicit def toJson(state:WikiHopState):JValue = {
    ("features" -> state.toFeatures) ~
      ("candidates" -> state.candidateEntities.get) ~
      ("candidatesTypes" -> state.candidateEntitiesTypes.get) ~
      ("candidatesOrigins" -> state.candidateEntitiesOrigins.get) ~
      ("iterationsOfIntroduction" -> state.iterationsOfIntroduction) ~
      ("ranks" -> state.ranks) ~
      ("entityUsage" -> state.ranks) ~
      ("exploreScores" -> state.exploreScores) ~
      ("exploitScores" -> state.exploitScores) ~
      ("sameComponents" -> state.pairwiseComponents)
  }
}
