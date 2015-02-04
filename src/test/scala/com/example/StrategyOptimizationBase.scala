package com.example

import com.example.loto.actors.{LogIterationResults, LoggerActor}
import com.example.loto.metrics.{MoneyHitStatisticsType, StrategyStatistics}
import com.example.loto.{SimpleParallelSort, StrategyWithMoneyStatistics}
import com.example.loto.model.RunResult

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by alespuh on 12.01.15.
 */
trait StrategyOptimizationBase
{
    type PastWindow = Int; type SkipWindow = Int; type FutureWindow = Int
    type Hit2 = Int; type Hit3 = Int; type Hit4 = Int; type Hit5 = Int
    type MoneyPlus = Int; type MoneyMinus = Int
    type Statistics = (PastWindow, SkipWindow, FutureWindow, Hit2, Hit3, Hit4, Hit5, MoneyPlus, MoneyMinus)

    def testStrategy[TFrom, TStrategy <: StrategyWithMoneyStatistics[TFrom, Array[Int]]](strategy: TStrategy, chunkSize: Int)(
        betGenerator: (TStrategy, TFrom) => Array[Int])(intersectionStatistics: (Array[RunResult], Array[Int]) => (StrategyStatistics, MoneyHitStatisticsType#SliceSize)): IndexedSeq[Statistics] =
    {
        val pRangeStart = 8
        val pRangeSize = (chunkSize * 4 - pRangeStart) / 4
        val pRangeRemainder = (chunkSize * 4 - pRangeStart) % 4
        val pRange1 = pRangeStart to pRangeSize
        val pRange2 = (pRangeSize + 1) to 2 * pRangeSize
        val pRange3 = (2 * pRangeSize + 1) to 3 * pRangeSize
        val pRange4 = (3 * pRangeSize + 1) to (4 * pRangeSize + pRangeRemainder)
        val sRange = 0 to pRangeSize * 4
        val fRangeStart = 1
        val fRange = fRangeStart to pRangeSize * 4

        val sorter = new SimpleParallelSort[Statistics, (Hit5, Hit4)](4, 50, (0, 0, 0, 0, 0, 0, 0, 0, 0))(x => (x._7, x._6))

        def calcFragment(pRange: Range, fragment: Int) =
        {
            for(pw <- pRange; sw <- sRange; fw <- fRange)
            {
                if(pw % 10 == 0 && sw == 0 && fw == fRangeStart) println("+")
                val strategyResult = strategy.apply(pw, sw, fw)(rrs => betGenerator(strategy, rrs))(intersectionStatistics)
                val StrategyStatistics(hit2, hit3, hit4, hit5, mplus, mminus) =
                    StrategyStatistics.aggregateStatistics(strategyResult)
                sorter.update(fragment, (pw, sw, fw, hit2, hit3, hit4, hit5, mplus, mminus))
            }
        }

        calcFutureResult(sorter, calcFragment(pRange1, 0), calcFragment(pRange2, 1), calcFragment(pRange3, 2), calcFragment(pRange4, 3))
    }

    def calcFutureResult(sorter: SimpleParallelSort[Statistics, _], body1: => Unit, body2: => Unit, body3: => Unit, body4: => Unit) =
    {
        val future1 = Future { body1 }
        val future2 = Future { body2 }
        val future3 = Future { body3 }
        val future4 = Future { body4 }
        val resultFuture = for{ result1 <- future1; result2 <- future2; result3 <- future3; result4 <- future4 } yield
        {
            sorter.result
        }
        Await.result(resultFuture, Duration.Inf).filter(x => x._8 > 0)
    }

    def accumulateResult[T](prefix: T, result: List[(T, Statistics)], append: Seq[Statistics]) =
    {
        val newResult = (result ++ append.map(x => (prefix, x))).sortBy{ case (_, x) => (x._7, x._6, x._5) }.reverse.take(200)
        LoggerActor.ref ! LogIterationResults(prefix.toString, append.map(_.toString()))
        newResult
    }
}
