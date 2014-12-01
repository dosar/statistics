package com.example

/**
 * Created by alex on 30.11.14.
 */
trait TestHelper {

  def genSeqs(from: Int, to: Int, seq : Seq[Int]): Seq[Seq[Int]] =
  {
    if(from==to)
      seq::Nil
    else
      genSeqs(from+1, to, seq.drop(from)) :+ seq.take(from)
  }

}
