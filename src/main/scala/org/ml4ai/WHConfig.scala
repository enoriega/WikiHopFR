package org.ml4ai

import com.typesafe.config.ConfigFactory

import scala.util.{Failure, Success, Try}

object WHConfig {
  private val config = ConfigFactory.load

  object Files {
    private val f = config.getConfig("files")

    val trainingPath: String = f.getString("trainingPath")
    val testingPath: String = f.getString("testingPath")
    val annotationsFile: String = f.getString("annotationsFile")
    val openIEAnnotationsFile: String = f.getString("openIEAnnotationsFile")
    val entityListFile: String = f.getString("entityListFile")
    val graphvizDir: String = f.getString("graphvizDir")
    val benchmarkOutput: String = f.getString("benchmarkOutput")
  }

  object PathFinder {
    private val f = config.getConfig("pathFinder")
    val knowledgeGraphType: String = f.getString("knowledgeGraphType")
    val outputFile: String = f.getString("outputFile")
  }

  object Lucene {
    val directoryIndex: String = config.getString("lucene.directoryIndex")
  }

  object Embeddings {
    private val f = config.getConfig("embeddings")

    val dimensions: Int = f.getInt("dimensions")
    val model: String = f.getString("model")
    val binaryMatrix: Boolean = f.getBoolean("binaryMatrix")
    val binaryPath: String = f.getString("binaryPath")
    val threads: Int = f.getInt("threads")
    val embeddingsFile: String = f.getString("embeddingsFile")
  }

  object Environment {
    private val f = config.getConfig("environment")

    val knowledgeGraphType: String = f.getString("knowledgeGraphType")
    val documentUniverse: String = f.getString("documentUniverse")
    val successReward: Double = f.getDouble("successReward")
    val failureReward: Double = f.getDouble("failureReward")
  }

  object Benchmark {
    private val f = config.getConfig("benchmark")

    val agentType: String = f.getString("agentType")

    val totalWorkers:Option[Int] = Try(f.getInt("totalWorkers")) match {
      case Success(tw) => Some(tw)
      case Failure(_) => None
    }

    val workerIndex:Option[Int] = Try(f.getInt("workerIndex")) match {
      case Success(wi) => Some(wi)
      case Failure(_) => None
    }
  }
}
