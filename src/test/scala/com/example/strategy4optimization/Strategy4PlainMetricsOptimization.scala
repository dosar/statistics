package com.example.strategy4optimization

import com.example.StrategyOptimizationBase
import com.example.loto.Strategy4
import com.example.loto.model.{RunResult, RunResults}
import org.scalatest.FunSuite

/**
 * Created by alespuh on 31.01.15.
 */
class Strategy4PlainMetricsOptimization extends FunSuite with StrategyOptimizationBase
{
//    ((6,2,36),(11,8,77,218,27,5,2,2088890,233100))
//    ((6,15,36),(21,86,17,202,25,5,2,2084990,217260))
    test("optimize strategy4 topNonZeroFiguresGeneric")
    {
        testStrategy4PlainMetrics(_.topNonZeroFiguresGeneric(_))
    }

//    ((6,15,36),(24,23,85,189,28,2,3,3064800,228060))
//    ((6,14,36),(41,64,58,209,25,1,3,3057030,217620))
//    ((6,9,36),(18,43,30,196,20,1,3,3050520,225540))
    test("optimize strategy4 fromMiddleOccurencies")
    {
        testStrategy4PlainMetrics(_.fromMiddleOccurencies(_))
    }

    /* лучшая стратега на данный момент */
//    ((6,4,36),(34,74,59,171,31,1,4,4058410,217080))
//    ((6,4,36),(34,56,59,186,28,1,4,4057240,220320))
//    ((6,12,36),(84,22,41,194,29,4,3,3080790,217440))
    test("optimize strategy4 middleOccurencyFigures")
    {
        testStrategy4PlainMetrics(_.middleOccurencyFigures(_))
    }

//    ((6,3,36),(10,58,92,201,29,4,2,2081630,224280))
//    ((6,2,36),(11,30,54,210,30,2,2,2066210,204840))
    test("optimize strategy4 zeroOccurencyFigures")
    {
        testStrategy4PlainMetrics(_.zeroOccurencyFigures(_))
    }

    /* эта шляпа стабильно не приносит хороших результатов */
//    ((6,17,36),(18,6,86,215,21,3,3,3068190,232200))
//    ((6,16,36),(18,6,86,215,21,2,3,3060990,232200))
    test("optimize strategy4 topNonZeroFiguresGenericIS")
    {
        testStrategy4PlainMetrics(_.topNonZeroFiguresGenericIS(_))
    }

    test("optimize strategy4 topNonZeroFiguresGenericQS")
    {
        testStrategy4PlainMetrics(_.topNonZeroFiguresGenericQS(_))
    }

//    ((6,9,36),(16,11,53,200,29,5,3,3088710,231660))
//    ((6,16,36),(12,9,5,233,30,4,3,3086460,232740))
//    ((6,1,36),(12,40,85,230,39,2,3,3080610,227160))
    test("optimize strategy4 leastPopularFigures")
    {
        testStrategy4PlainMetrics(_.leastPopularFigures(_))
    }

    def testStrategy4PlainMetrics(metric: (Strategy4, Array[RunResult]) => Array[Int]) =
    {
        var result = List[((Int, Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17; endFigure <- 36 to 36)
        {
            println((betSize, startFigure, endFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, endFigure)
            def test = testStrategy[Array[RunResult], Strategy4](strategy, 25)(metric) _
            result = accumulateResult((betSize, startFigure, endFigure), result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }
}
