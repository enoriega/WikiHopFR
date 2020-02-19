package org.ml4ai.ir

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.NIOFSDirectory
import org.ml4ai.WHConfig
import org.ml4ai.ir.LuceneHelper.{addToIndex, analyzer}
import org.ml4ai.utils.{HotPotParser, WikiHopParser}


object HotPotIndexerApp extends App with LazyLogging {

  val indexDir = new File(WHConfig.Lucene.HotPotQA.directoryIndex)

  if(!indexDir.exists()){
    indexDir.mkdirs()
  }

  val documents = HotPotParser.allDocuments
  val index = new NIOFSDirectory(indexDir.toPath)

  val ixWConfig = new IndexWriterConfig(analyzer)

  val w = new IndexWriter(index, ixWConfig)

  documents.zipWithIndex foreach {
    case ((title, contents), i) =>
      logger.info(s"indexing $i")
      addToIndex(w, contents)
  }

  w.commit()
  w.close()

}
