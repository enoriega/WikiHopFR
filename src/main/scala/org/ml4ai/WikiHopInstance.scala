package org.ml4ai

abstract class Instance(id:String, query:String, answer:Option[String], supportDocs:Seq[String])

case class WikiHopInstance(id:String,
                           query:String,
                           answer:Option[String],
                           candidates:Seq[String],
                           supportDocs:Seq[String]) extends Instance(id, query, answer , supportDocs)

case class HotPotInstance(id:String,
                          question:String,
                          query:String,
                          answer:Option[String],
                          supportDocs:Seq[String]) extends Instance(id, query, answer , supportDocs)