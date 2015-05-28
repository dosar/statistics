package com.example.strategy4optimization

import com.example.StrategyOptimizationBase
import com.example.loto._
import com.example.loto.metrics._
import com.example.loto.model.{RunResult, RunResults}
import org.scalatest.FunSuite

import scala.collection.immutable.IndexedSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class Strategy4Optimization extends FunSuite with StrategyOptimizationBase
{
    test("optimize strategy4 probabalisticIntervalFigures getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with ProbabalisticIntervalsMetrics
            {
                override val probabilities: Array[Double] = Array(1.0, 0.7, 0.7, 0.7, 1.0)
            }
            def test = testStrategy[Array[RunResult], Strategy4 with ProbabalisticIntervalsMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.probabalisticIntervalFigures(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    /* хороша по хитам в 4ки*/
    test("optimize strategy4 topPairFigures")
    {
        var result = List[((Int, Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 4; endFigure <- 33 to 36)
        {
            println((betSize, startFigure, endFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, endFigure) with PairFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy4 with PairFigureMetrics](strategy, 25)(_.topPairFigures(_)) _
            result = accumulateResult((betSize, startFigure, endFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy4 topFigureCombinedWithPair")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with PairFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy4 with PairFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.topFigureCombinedWithPair(_))(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    def testStrategy1(betGenerator: (Strategy1, Array[RunResult]) => Array[Int]): Unit =
        testStrategy(new Strategy1(RunResults.runResults), betGenerator)

    def testStrategy3(topFiguresCount: Int, startFigure: Int, endFigure: Int)(betGenerator: (Strategy3, Array[RunResult]) => Array[Int]) =
        testStrategy(new Strategy3(RunResults.runResults, topFiguresCount, startFigure, endFigure), betGenerator)

    def testStrategy2(startFigure: Int = 1)(extractor: (Strategy2, Array[RunResult]) => Array[(Int, Int)]): Unit =
    {
        val strategy = new Strategy2(RunResults.runResults, startFigure = startFigure)

        val pRange = 1 to 300
        val sRange = 0 to 300
        val fRange = 1 to 300
        val results = for(pw <- pRange.par; sw <- sRange.par; fw <- fRange.par) yield
        {
            if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
            val result = strategy.strategy2(pw, sw, fw)(rrs => extractor(strategy, rrs))(strategy.topNonZeroFiguresWithoutPrevious)
            (pw, sw, fw, result.count(_._2._1 == 5))
        }

        results.toList.sortBy(- _._4).take(200) foreach println
    }

    type Strategy =
    {
        def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(
            betGenerator: Array[RunResult] => Array[Int]): Seq[(Array[Int], (Int, Int))]
    }

    def testStrategy[TStrategy <: Strategy](strategy: TStrategy, betGenerator: (TStrategy, Array[RunResult]) => Array[Int]): IndexedSeq[(Int, Int, Int, Int)] =
    {
        val pRangeSize = 25
        val pRange1 = 1 to pRangeSize
        val pRange2 = (pRangeSize + 1) to 2 * pRangeSize
        val pRange3 = (2 * pRangeSize + 1) to 3 * pRangeSize
        val pRange4 = (3 * pRangeSize + 1) to 4 * pRangeSize
        val sRange = 0 to 100
        val fRange = 1 to 100

        val sorter = new SimpleParallelSort[(Int, Int, Int, Int), Int](4, 50, (0, 0, 0, 0))(_._4)

        def calcFragment(pRange: Range, fragment: Int) =
        {
            for(pw <- pRange; sw <- sRange; fw <- fRange)
            {
                if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
                sorter.update(fragment,
                    (pw, sw, fw, strategy(pw, sw, fw)(rrs => betGenerator(strategy, rrs)).count(_._2._1 == 5)))
            }
        }

        val future1 = Future { calcFragment(pRange1, 0) }
        val future2 = Future { calcFragment(pRange2, 1) }
        val future3 = Future { calcFragment(pRange3, 2) }
        val future4 = Future { calcFragment(pRange4, 3) }
        val result = for{result1 <- future1; result2 <- future2; result3 <- future3; result4 <- future4} yield
        {
            sorter.result
        }
        val awaitedResult = Await.result(result, 120 minutes)
        awaitedResult foreach println
        awaitedResult
    }
}