package org.ml4ai.ie

import org.ml4ai.{WHConfig, utils}

import scala.io.Source

case class WikificationEntry(docId:Int,
                             text:String,
                             start:Int,
                             end:Int,
                             sentence:Int,
                             canonicalName:String,
                             wikiId:Int)

object WikificationEntry {
  def apply(line:String): WikificationEntry = {
    val tokens = line.split("\t")
    WikificationEntry(tokens(0).toInt,
      tokens(1),
      tokens(2).toInt,
      tokens(3).toInt,
      tokens(4).toInt,
      tokens(5),
      tokens(6).toInt)
  }
}

object WikificationEntities extends App {


  def readWikificationDataBase():Map[String, Seq[WikificationEntry]] = {
    val path = WHConfig.Files.wikificationPath
    val indexPath = WHConfig.Files.wikificationIndexPath

    val index =
      utils.using(Source.fromFile(indexPath)){
        source =>
          source.getLines().map{l => val t = l.split("\t"); t(1).toInt -> t(0)}.toMap
      }

    val entries =
      utils.using(Source.fromFile(path)){
        source =>
          for(line <- source.getLines().toList)
            yield WikificationEntry(line)
      }

    entries.filter(e => index.contains(e.docId)).groupBy(e => index(e.docId))
  }

}
