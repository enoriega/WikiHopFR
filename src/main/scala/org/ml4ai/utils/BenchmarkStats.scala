package org.ml4ai.utils

import java.io.PrintWriter

import org.ml4ai.agents.StatsObserver
import org.ml4ai.inference.VerboseRelation
import org.json4s.NoTypeHints
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write

import scala.collection.mutable
import scala.util.{Try, Success}

case class StatsDatum(instanceId:String, paths:Iterable[Seq[VerboseRelation]], observer:StatsObserver) extends Serializable

/**
  * Main analysis source file for the results of a benchmark run
  */
class BenchmarkStats(data:Iterable[StatsDatum])  extends Serializable {

  val size: Int = data.size

  private lazy val (successes, failures) = data partition {
    case StatsDatum(_, p, _) =>
      p.nonEmpty
  }

  // Queries
  lazy val numSuccesses: Int = successes.size
  lazy val numFailures: Int = failures.size

  lazy val successRate: Double = numSuccesses.toDouble / size

  private def crunchNumbers(observers:Iterable[StatsObserver]) = {
    val iterationNumDistribution = observers.map(_.iterations.get).groupBy(identity).mapValues(_.size)
    val papersDistribution = observers.map(_.papersRead.get).groupBy(identity).mapValues(_.size)
    val acc = new mutable.HashMap[String, Int].withDefaultValue(0)
    val concAcc = new mutable.HashMap[String, Int].withDefaultValue(0)
    val docsCon = new mutable.HashMap[String, Int].withDefaultValue(0)

    for(curr <- observers.map(_.actionDistribution)){
      acc("EXPLORATION") += curr(StatsObserver.EXPLORATION)
      acc("EXPLORATION_DOUBLE") += curr(StatsObserver.EXPLORATION_DOUBLE)
      acc("EXPLOITATION") += curr(StatsObserver.EXPLOITATION)
      acc("RANDOM") += curr(StatsObserver.RANDOM)
      acc("CASCADE") += curr(StatsObserver.CASCADE)
    }

    for(curr <- observers.map(_.concreteActionDistribution)){
      concAcc("EXPLORATION") += curr(StatsObserver.EXPLORATION)
      concAcc("EXPLORATION_DOUBLE") += curr(StatsObserver.EXPLORATION_DOUBLE)
      concAcc("EXPLOITATION") += curr(StatsObserver.EXPLOITATION)
    }

    for(curr <- observers.map(_.documentsContribution)){
      docsCon("EXPLORATION") += curr(StatsObserver.EXPLORATION)
      docsCon("EXPLORATION_DOUBLE") += curr(StatsObserver.EXPLORATION_DOUBLE)
      docsCon("EXPLOITATION") += curr(StatsObserver.EXPLOITATION)
    }

    (iterationNumDistribution, papersDistribution, acc.toMap, concAcc.toMap, docsCon.toMap)
  }

  @transient private lazy val numbers = crunchNumbers(successes map (_.observer))

  lazy val iterationNumDistribution: Map[Int, Int] = numbers._1.mapValues(identity)
  lazy val papersDistribution: Map[Int, Int] = numbers._2.mapValues(identity)
  lazy val actionDistribution: Map[String, Int] = numbers._3.mapValues(identity)
  lazy val concreteActionDist: Map[String, Int] = numbers._4.mapValues(identity)
  lazy val documentsContribution: Map[String, Int] = numbers._5.mapValues(identity)

  def toJson(path:String) {

    implicit val formats = Serialization.formats(NoTypeHints)

    using(new PrintWriter(path)){
      pw =>
        pw.print(
          write(
            Map(
              "size" -> size,
              "numSuccesses" -> numSuccesses,
              "numFailures" -> numFailures,
              "stats" -> Map(
                "iterationsDist" -> iterationNumDistribution,
                "papersDist" -> papersDistribution,
                "actionDist" -> actionDistribution,
                "concreteActionDist" -> concreteActionDist,
                "documentContributions" -> documentsContribution
              ),
              "data" -> data.map{
                d =>
                  Try {
                    Map(
                      "id" -> d.instanceId,
                      "success" -> (if (d.paths.nonEmpty) true else false),
                      "paths" -> d.paths,
                      "iterations" -> d.observer.iterations.get,
                      "papersRead" -> d.observer.papersRead.get,
                      "actions" -> d.observer.actionDistribution.toMap,
                      "errors" -> (d.observer.errors map {
                        ex =>
                          Map(
                            "type" -> ex.toString,
                            "message" -> (if (ex.getMessage != null) ex.getMessage else "")
                          )
                      })
                    )
                  }
              }.collect{ case Success(m) => m }
            )
          )
        )
    }
  }

}
