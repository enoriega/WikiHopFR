package org.ml4ai

import java.security.MessageDigest

import org.clulab.processors.shallownlp.ShallowNLPProcessor

import scala.collection.mutable
import scala.io.Source
import scala.language.reflectiveCalls
import scala.util.hashing.MurmurHash3
import scala.util.Random

package object utils {

  def using[A <: { def close() }, B](resource:A)(func: A => B): B = {
    val returnValue:B = func(resource)
    resource.close()
    returnValue
  }

  def md5Hash(s:String):String = {
    val md5 = MessageDigest.getInstance("md5")
    md5.digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }

  def all(elems:Traversable[Boolean]):Boolean = if(elems.isEmpty) false else elems.reduce((a, b) => a & b)
  def any(elems:Traversable[Boolean]):Boolean = if(elems.isEmpty) false else elems.reduce((a, b) => a | b)

  lazy val stopWords:Set[String] = using(Source.fromURL(getClass.getResource("/stopwords.txt"))){
    s =>
      s.getLines.toSet
  }

  lazy val shallowProcessor = new ShallowNLPProcessor

  def lemmatize(words:Seq[String]):Seq[String] = {
    val doc = shallowProcessor.mkDocumentFromTokens(Seq(words))
    shallowProcessor.annotate(doc)
    doc.sentences.flatMap(s => filterUselessLemmas(s.lemmas.get))
  }

  def lemmatize(words:String):Seq[String] = lemmatize(words.split(" ").toSeq)

  private lazy val stopLemmas:Set[String] = {
    val proc = shallowProcessor
    val doc  = proc.mkDocumentFromTokens(stopWords map (Seq(_)))
    proc.annotate(doc)
    doc.sentences.flatMap(_.lemmas.get).toSet
  }

  def entityGroundingHash(lemmas:Iterable[String]):Int = {
    val filteredLemmas = filterUselessLemmas(lemmas).toSet
    if(filteredLemmas.isEmpty)
      0
    else
      MurmurHash3.unorderedHash(filterUselessLemmas(lemmas))
  }

  def filterUselessLemmas(lemmas: Iterable[String]): Iterable[String] = lemmas.map(_.toLowerCase).filter(!stopLemmas.contains(_))

  // Set random seed to our global random number generator
  def buildRandom(seed:Int = WHConfig.Random.seed):Random = new Random(seed)

  // Global random number generator
  implicit lazy val rng: Random = buildRandom()

  def prettyPrintMap[K](m:Map[K, Int]):String = {
    val buf = new mutable.StringBuilder("\n")
    val entries = m.toSeq.sortBy{case (k, v) => v}.reverse
    entries foreach {
      case (k, v) =>
        buf.append(s"$k:\t$v\n")
    }
    buf.toString()
  }

  def sigmoid(x:Double):Double = 1.0 / (1.0 + Math.exp(-x))

}
