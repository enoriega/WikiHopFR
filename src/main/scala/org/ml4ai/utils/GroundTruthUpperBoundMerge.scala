package org.ml4ai.utils

import java.io.PrintWriter

import scala.collection.mutable
import scala.io.Source

case class Entry(source:Int, dest:Int, attrs:Iterable[(String, Int)]) {
  override def toString: String = {
    val a = attrs map {
      case (d, s) =>
        s"$d-$s"
    }

    s"$source\t$dest\t${a.mkString(", ")}\n"
  }
}

object Entry {
  def apply(s:String): Entry = {
    val tokens = s.split("\t")
    val source = tokens(0).toInt
    val dest = tokens(1).toInt
    val rawAttrs = tokens(2).split(", ")
    val attrs = rawAttrs map {
      a =>
        val t = a.split("-")
        (t(0), t(1).toInt)
    }
    Entry(source, dest, attrs)
  }

  def readFromFile(path:String):Iterable[Entry] = {
    using(Source.fromFile(path)){
      source =>
        val lines = source.getLines()
        (lines map Entry.apply).toList
    }
  }
}

object GroundTruthUpperBoundMerge extends App {

  val foldA = args(0).toInt
  val foldB = args(1).toInt

  def merge(a:String, b:String):Iterable[Entry] = {
    val entriesA = Entry.readFromFile(a)
    val entriesB = Entry.readFromFile(b)

    val (longer, shorter) =
      if(entriesA.size > entriesB.size)
        (entriesA, entriesB)
      else
        (entriesB, entriesA)

    val longerMap =
      longer.par.map{
        e =>
          val a = mutable.ListBuffer[(String, Int)]()
          a ++= e.attrs
          (e.source, e.dest) -> a
      }.toMap

    val cache =
      mutable.Map() ++ longerMap

    for(e <- shorter){
      val key = (e.source, e.dest)
      if(!cache.contains(key))
        cache += key -> mutable.ListBuffer[(String, Int)]()
      cache(key) ++= e.attrs
    }

    val ret =
      cache map {
        case ((s, d), attrs) =>
          Entry(s, d, attrs.toList)
    }

    ret.seq
  }

  val merged = merge(s"edges_$foldA.txt", s"edges_$foldB.txt")

  using(new PrintWriter(s"edges_${foldA}_$foldB.txt")){
    pw =>
      merged foreach (e => pw.write(e.toString))
  }
}
