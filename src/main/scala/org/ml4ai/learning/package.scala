package org.ml4ai

//import edu.cmu.dynet.{Expression, Tensor, UnsignedVector}
//
//package object learning {
//  def argMax(values:Tensor):Seq[Int] = {
//    val columns = values.toSeq().grouped(values.getD().rows().toInt)
//    columns.map{
//      col =>
//        col.zipWithIndex.maxBy(_._1)._2
//    }.toSeq
//  }
//
//  def max(values:Tensor):Seq[Float] = {
//    val columns = values.toSeq().grouped(values.getD().rows().toInt)
//    columns.map{
//      col =>
//        col.max
//    }.toSeq
//  }
//
//  def mseLoss(pred:Expression, target:Expression): Expression = {
//    val diff = pred - target
//    val square = Expression.sumDim(Expression.square(diff), UnsignedVector.Seq2UnsignedVector(Seq(1)))
//    val mean = Expression.meanElems(square)
//    mean
//  }
//}
