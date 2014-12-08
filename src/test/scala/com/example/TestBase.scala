package com.example

import java.util.Date

import com.example.loto.model.RunResult
import org.scalatest._

trait TestBase extends FunSuite
{
    implicit def rr(result: (Int, Int, Int, Int, Int)) =
        RunResult(0, new Date(), result.productIterator.map(_.asInstanceOf[Int]).toArray)

    protected def check[T](left: Seq[(Seq[Int], T)], right: Seq[(Seq[Int], T)]) =
    {
        def toUnordered(seq: Seq[(Seq[Int], T)]) = seq.map{case (innerSeq, count) => (innerSeq.toSet, count)}
        assert(toUnordered(left) === toUnordered(right))
    }

    protected def checkArr[T](left: Seq[(Array[Int], T)], right: Seq[(Array[Int], T)]) =
    {
        def toUnordered(seq: Seq[(Array[Int], T)]) = seq.map{case (innerSeq, count) => (innerSeq.toSet, count)}
        assert(toUnordered(left) === toUnordered(right))
    }
}
