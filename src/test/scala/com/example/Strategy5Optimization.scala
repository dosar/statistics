package com.example

import com.example.loto._
import com.example.loto.actors.{LogResults, LoggerActor}
import com.example.loto.model.{RunResult, RunResults}
import org.scalatest.FunSuite

/**
 * Created by alespuh on 12.01.15.
 */
class Strategy5Optimization extends FunSuite with StrategyOptimizationBase
{
    test("optimize strategy5 topNonZeroFiguresGeneric")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36)
            def test = testStrategy[Array[RunResult], Strategy5](strategy, 25)(_.topNonZeroFiguresGeneric(_)) _
            result = accumulateResult((betSize, startFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /*тоже есть 4 хита в 5ку*/
    test("optimize strategy5 topWithMiddle getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy5 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.topWithMiddle(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    /*3 хита*/
    test("optimize strategy5 topWithTrueMiddle getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy5 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.topWithTrueMiddle(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    /*3 хита*/
    test("optimize strategy5 topWithZero getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy5 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.topWithZero(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy5 middleWithTrueMiddle getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 10 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy5 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.middleWithTrueMiddle(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy5 middleWithZero getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy5 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.middleWithZero(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy5 trueMiddleWithZero getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy5 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.trueMiddleWithZero(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    /* тут тоже не очень */
    test("optimize strategy5 fromMiddleOccurencies")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        val rrs = RunResults.runResults
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(rrs, betSize, startFigure, 36)
            def test = testStrategy[Array[RunResult], Strategy5](strategy, 25)(_.fromMiddleOccurencies(_)) _
            result = accumulateResult((betSize, startFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /* лучшая стратега на данный момент */
    test("optimize strategy5 middleOccurencyFigures")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36)
            def test = testStrategy[Array[RunResult], Strategy5](strategy, 10)(_.middleOccurencyFigures(_)) _
            result = accumulateResult((betSize, startFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /* тут тоже не очень */
    test("optimize strategy5 zeroOccurencyFigures")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36)
            def test = testStrategy[Array[RunResult], Strategy5](strategy, 25)(_.zeroOccurencyFigures(_)) _
            result = accumulateResult((betSize, startFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /* эта шляпа стабильно не приносит хороших результатов */
    test("optimize strategy5 topNonZeroFiguresGeneric1")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36)
            def test = testStrategy[Array[RunResult], Strategy5](strategy, 25)(_.topNonZeroFiguresGeneric1(_)) _
            result = accumulateResult((betSize, startFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy5 topNonZeroFiguresExceptSome")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        val nonPopularFigures = Array(36, 30, 35, 27, 4, 32, 6, 15, 26, 33, 1, 10, 19, 22, 2, 28, 7, 18, 20, 31, 8, 16,
            25, 23, 13, 24, 5, 21, 3, 34, 17, 29, 14, 12, 9, 11).reverse
        val rrs = RunResults.runResults
        for(betSize <- 6 to 6; ignoredSize <- 1 to 17)
        {
            println((betSize, ignoredSize))
            val nonUseful = nonPopularFigures.take(ignoredSize).sorted :+ 0
            val strategy = new Strategy5(rrs, betSize, 1, 36)
            def test = testStrategy[Array[RunResult], Strategy5](strategy, 25)(_.topNonZeroFiguresExceptSome(_, nonUseful)) _
            result = accumulateResult((betSize, ignoredSize), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy5 probabalisticIntervalFigures getCombinedBetIntersectionStatistics")
    {
        LoggerActor.ref ! LogResults(Seq("optimize strategy5 probabalisticIntervalFigures getCombinedBetIntersectionStatistics"))
        var result = List[((Int, Int, Double, Double, Double, Double, Double), Statistics)]()
        val pRange = Seq(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
        for(betSize <- 6 to 6; startFigure <- 1 to 17; p1 <- pRange)
        {
            for(p2 <- pRange; p3 <- pRange; p4 <- pRange; p5 <- pRange)
            {
                val prefix = (betSize, startFigure, p1, p2, p3, p4, p5)
                println(prefix)
                val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with ProbabalisticIntervalsMetrics
                {
                    override val probabilities: Array[Double] = Array(p1, p2, p3, p4, p5)
                }
                def test = testStrategy[Array[RunResult], Strategy5 with ProbabalisticIntervalsMetrics](strategy, 10) _
                result = accumulateResult(prefix, result,
                    test(_.probabalisticIntervalFigures(_))(strategy.getCombinedBetIntersectionStatistics))
            }
            LoggerActor.ref ! LogResults(result.map(_.toString()))
        }
        result foreach println
        LoggerActor.ref ! LogResults(result.map(_.toString()))
    }

    /* хороша по хитам в 4ки*/
    test("optimize strategy5 topPairFigures")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with PairFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy5 with PairFigureMetrics](strategy, 25)(_.topPairFigures(_)) _
            result = accumulateResult((betSize, startFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy5 topFigureCombinedWithPair")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36) with PairFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy5 with PairFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure), result, test(_.topFigureCombinedWithPair(_))(strategy.getIntersectionStatistics))
        }
        result foreach println
    }
}
