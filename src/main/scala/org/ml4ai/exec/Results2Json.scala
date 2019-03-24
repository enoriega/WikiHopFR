package org.ml4ai.exec

import java.io.PrintWriter

import com.typesafe.config.ConfigFactory
import org.clulab.utils.Serializer
import org.json4s.NoTypeHints
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import org.ml4ai.exec.PathFinder.{NoPaths, Outcome, Successful, Unsuccessful}
import org.ml4ai.utils.{md5Hash, using}

object Results2Json extends App {

  val config = ConfigFactory.load()

  val inPath = config.getString("pathFinder.outputFile")

  val outPath = inPath.split("\\.").dropRight(1).mkString(".") + ".json"

  val data = Serializer.load[Map[String, Outcome]](inPath)

  implicit val formats = Serialization.formats(NoTypeHints)

  using(new PrintWriter(outPath)){
    pw =>
      pw.print(
        write(
          data.mapValues{
            case Successful(paths) => Map("Successful" -> (paths map {
              elems =>
                elems map {
                  e => Map("Source" -> e.source,
                    "Destination" -> e.destination,
                    "Attributions" -> e.attributions.map{
                      a =>
                        Map("Doc" -> md5Hash(a.document), "Sentence" -> a.sentenceIx)
                    })
                }
            }))
            case NoPaths => "NoPath"
            case Unsuccessful(e) => Map("Error" -> e.toString)
          }
        )
      )
  }

}