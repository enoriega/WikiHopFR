package org.ml4ai.utils

import java.io.PrintWriter

import org.clulab.odin.TextBoundMention
import org.ml4ai.WHConfig
import org.ml4ai.ie.OdinEngine

import scala.collection.mutable

object NamedEntityEnumerator extends App {
  private val annotationsPath = WHConfig.Files.annotationsFile
  //private val annotationsPath = WHConfig.Files.HotPotQA.annotationsFile

  val loader = new AnnotationsLoader(annotationsPath)

  val keys = loader.rawDocuments.toList

  case class NamedEntity(text:String, kind:String)

  val seen = new mutable.HashSet[NamedEntity]

  val pw = new PrintWriter("wikihop_entities.tsv")

  var ix = 0

  for(k <- keys) {
    val d = loader(k)
    val entities =
      OdinEngine.extractorEngine.extractFrom(d).collect {
        case tb: TextBoundMention if tb.labels.contains("Entity") => tb
      }

    entities foreach {
      e =>
        val entity = NamedEntity(e.text, e.label)
//        if(!seen.contains(entity)){
          seen += entity
          val s = s"${e.text}\t${e.label}"
          println(s"$ix. " + s)
          ix += 1
          pw.println(s)
//        }

    }
  }

  pw.close()

}
