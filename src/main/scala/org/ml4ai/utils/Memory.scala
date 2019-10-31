package org.ml4ai.utils

import org.ml4ai.learning.Transition

import scala.collection.mutable
import scala.util.Random

class Memory[A](val maxSize:Int = 100000) extends mutable.Queue[A]{

  def remember(transitions:Iterable[A]):Unit = {
    val remainingSlots = maxSize - this.size
    if(remainingSlots < transitions.size){
      val requiredSlots = transitions.size - remainingSlots
      for(_ <- 0 until requiredSlots)
        dequeue()
    }
    this ++= transitions
  }

  def remember(transition:A): Unit = remember(Iterable(transition))

  def sample(size:Int)(implicit rng:Random):Iterable[A] = {
    if(this.size < size)
      this
    else {
      // This code does sample with replacement, however with a large enough memory it should be very unlikely to happen
      // and shouldn't be a big issue for training
      val ceil = Seq(maxSize, this.size).min
      val indices = (0 until size) map (_ => rng.nextInt(ceil))
      indices map this
    }
  }
}

class TransitionMemory(override val maxSize:Int = 100000) extends Memory[Transition] {

  def sampleWithReward(size:Int, p:Float)(implicit rng:Random):Iterable[Transition] = {
    val (withReward, noReward) = this.partition(t => t.reward != 0)
    val amountWReard = size * p

    val tempMemoryReward = new TransitionMemory(maxSize)
    tempMemoryReward remember withReward

    val tempMemoryNoReward = new TransitionMemory(maxSize)
    tempMemoryNoReward remember noReward

    val rewardPortion = tempMemoryReward.sample(Math.ceil(amountWReard).toInt)

    if(rewardPortion.size < amountWReard)
      rewardPortion
    else
      rewardPortion ++ tempMemoryNoReward.sample(Math.floor(size - amountWReard).toInt)
  }
}