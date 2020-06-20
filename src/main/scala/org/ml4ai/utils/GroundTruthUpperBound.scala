package org.ml4ai.utils

import org.clulab.utils.Serializer
import org.ml4ai.ie.{WikificationEntities, WikificationEntry}
import org.ml4ai.inference.{AttributingElement, KBLabel, Relation}
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._

import scala.collection.mutable

object GroundTruthUpperBound extends App {

  // First read the wikification DB
  println("Parsing input files")
  val wikificationEntities = WikificationEntities.readWikificationDataBase()

  println(wikificationEntities.size)

  // Now put together the edges of entities
  println("Building the relations")

  val groupedEntries =
    wikificationEntities.keys.flatMap{
      docId =>
        var maxSent = 0
        for(entry <- wikificationEntities(docId)){
          if(entry.sentence > maxSent)
            maxSent = entry.sentence
        }
        (0 to maxSent) map (six => (docId, six) -> mutable.ListBuffer[WikificationEntry]())
    }.toMap

  println("Generating the edges")
  for((docHash, entries) <- wikificationEntities.par){
    for(entry <- entries){
      val key = (docHash, entry.sentence)
      groupedEntries(key) += entry
    }
  }

  val entriesSize = groupedEntries.keys.toList.size

  val edges = new mutable.HashMap[(Int, Int), mutable.ListBuffer[AttributingElement]]()


  var ix = 0
  for(((docHash, senIx), entries) <- groupedEntries) {
    if((ix + 1)% 10000 == 0){
      println(s"Elem ${ix + 1} of $entriesSize")
    }
    val observed = mutable.HashSet[(Int, Int)]()
    for{
      a <- entries
      b <- entries
      if a.wikiId != b.wikiId
    } {
      val edgeKey = (a.wikiId, b.wikiId)
      if(!observed.contains(edgeKey)){
        if(!edges.contains(edgeKey)){
          edges += edgeKey -> new mutable.ListBuffer[AttributingElement]
        }
        val attr = AttributingElement(None, senIx, docHash)
        edges(edgeKey) += attr
      }
    }
    ix += 1
  }

  // Build THE global graph
  // Build graph
  println("Building the Graph")
  val graph: Graph[Int, UnDiEdge] = Graph.from(edges = edges.map{
    case((source, dest), _) =>
      source ~ dest
  }.seq)

  print(graph.size)

  Serializer.save(graph, "gt_graph.ser")
}
