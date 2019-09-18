package org.ml4ai.learning

import edu.cmu.dynet.{ComputationGraph, Dim, Expression, ExpressionVector, FloatVector, ParameterCollection, Tensor}
import org.ml4ai.mdp.{Exploitation, Exploration, ExplorationDouble, WikiHopState}
import org.sarsamora.actions.Action

class DQN(params:ParameterCollection, embeddingsHelper: EmbeddingsHelper) {

  private val embeddingsDimension = embeddingsHelper.embeddingsDim
  private val featuresDimension = WikiHopState.numFeatures
  private val featureVectorSize = 2*embeddingsDimension + featuresDimension

  private val pW = params.addParameters(Dim(10, featureVectorSize))
  private val pb = params.addParameters(Dim(10))
  private val pX = params.addParameters(Dim(2, 10))
  private val pc = params.addParameters(Dim(2))

  ComputationGraph.renew()

  def evaluate(input: (WikiHopState, Seq[String], Seq[String])): Tensor = evaluate(Seq(input))

  def evaluate(input: Seq[(WikiHopState, Seq[String], Seq[String])]): Tensor = {
    // It appears that this cannot be placed inside the loop below.
    val features = input.map { case (state, _, _) => vectorizeState(state) }

    val inputVectors = input.zip(features).map { case ((state, entityA, entityB), features) =>
      // val features = vectorizeState(state)
      val featureVector = Expression.input(Dim(features.size), features)
      val embA = embeddingsHelper.lookup(entityA)
      val embB = embeddingsHelper.lookup(entityB)

      Expression.concatenate(featureVector, aggregateEmbeddings(embA), aggregateEmbeddings(embB))
    }

    val expressionVector = new ExpressionVector(inputVectors)
    val inputMatrix = Expression.concatenateCols(expressionVector)
    val W = Expression.parameter(pW)
    val b = Expression.parameter(pb)
    val X = Expression.parameter(pX)
    val c = Expression.parameter(pc)
    val expr = X * Expression.tanh(W * inputMatrix + b) + c
    val value = expr.value()

    value
  }

  /**
    * Converts a state into a DyNET FloatVector
    * @param state instance
    * @return FloatVector to be used as instance
    */
  def vectorizeState(state:WikiHopState):FloatVector = {
    val features = Seq(
      state.iterationNum.toFloat,
      state.numNodes.toFloat,
      state.numEdges.toFloat
    )

    new FloatVector(features)
  }

  /**
    * Aggregates entity embeddings by certain criteria to yield a unique entity embedding
    */
  private def aggregateEmbeddings(embs:Seq[Expression]) = {
    // TODO: Add more nuance here
    Expression.average(embs)
  }
}

object DQN {
  import scala.language.implicitConversions

  implicit def actionIndex(action:Action):Int = action match {
    case _:Exploration => 0
    case _:ExplorationDouble => 1
    case _:Exploitation => 2
    case _ => throw new IllegalStateException("Unsupported action type for RL")
  }
}