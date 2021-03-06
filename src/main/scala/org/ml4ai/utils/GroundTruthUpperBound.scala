package org.ml4ai.utils

import java.io.PrintWriter

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


  val size= groupedEntries.keySet.size
  println(size)


  val edges = new mutable.HashMap[(Int, Int), mutable.ListBuffer[AttributingElement]]()


  val keys = groupedEntries.keys.toList.sorted
  val sliceNum = args(0).toInt - 1
  val sliceSize = 10000
  val offset = sliceNum * sliceSize
  val slice = keys.slice(offset, offset + sliceSize)
  val entriesSize = slice.size

  println(s"Slice $sliceNum")
  var ix = 0
  for((docHash, senIx) <- slice) {
    val entries = groupedEntries((docHash, senIx))
    if((ix + 1)% 1000 == 0){
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

  println("Saving result ...")

  val pw = new PrintWriter(s"edges_$sliceNum.txt")
  for(((source, dest), attributions) <- edges){
    val a = attributions.map(a => s"${a.document}-${a.sentenceIx}").mkString(", ")
    val s = s"$source\t$dest\t$a\n"
    pw.write(s)
  }

  pw.close()

//  Serializer.save(edges.map(identity), s"edges_$sliceNum.ser")
//
//  // Build THE global graph
//  // Build graph
//  println("Building the Graph")
//  val graph: Graph[Int, UnDiEdge] = Graph.from(edges = edges.map{
//    case((source, dest), _) =>
//      source ~ dest
//  }.seq)
//
//  print(graph.size)
//
//  Serializer.save(graph, "gt_graph.ser")
}
