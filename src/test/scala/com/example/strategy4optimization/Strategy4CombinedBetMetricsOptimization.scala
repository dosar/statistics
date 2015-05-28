package com.example.strategy4optimization

import com.example.StrategyOptimizationBase
import com.example.loto.Strategy4
import com.example.loto.metrics.CombinedBetFigureMetrics
import com.example.loto.model.{RunResult, RunResults}
import org.scalatest.FunSuite

/**
 * Created by alespuh on 31.01.15.
 */
class Strategy4CombinedBetMetricsOptimization extends FunSuite with StrategyOptimizationBase
{
//    ((6,4,32),(12,57,59,184,30,1,3,3058980,448200))
//    ((6,3,32),(19,80,58,185,17,6,2,2082230,437400))
    test("optimize strategy4 topWithMiddle getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.topWithMiddle(_))
    }

//    ((6,4,32),(12,57,59,184,30,1,3,3058980,448200))
//    ((6,3,32),(19,80,58,185,17,6,2,2082230,437400))
//    ((6,2,36),(11,8,77,218,27,5,2,2088890,466200))
    test("optimize strategy4 topWithTrueMiddle getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.topWithTrueMiddle(_))
    }

//    ((6,4,32),(12,57,59,184,30,1,3,3058980,448200))
//    ((6,3,32),(19,80,58,185,17,6,2,2082230,437400))
    test("optimize strategy4 topWithZero getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.topWithZero(_))
    }

//    ((6,4,32),(12,57,59,184,30,1,3,3058980,448200))
//    ((6,3,32),(19,80,58,185,17,6,2,2082230,437400))
//    ((6,2,36),(11,8,77,218,27,5,2,2088890,466200))
    test("optimize strategy4 topWithLeast getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.topWithLeast(_))
    }

//    ((6,4,36),(34,74,59,171,31,1,4,4058410,434160))
//    ((6,4,36),(34,56,59,186,28,1,4,4057240,440640))
//    ((6,5,33),(18,63,19,190,31,6,3,3096690,443880))
//    ((6,5,33),(18,44,90,193,17,6,3,3083190,450720))
    test("optimize strategy4 middleWithTrueMiddle getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.middleWithTrueMiddle(_))
    }

//    ((6,4,36),(34,74,59,171,31,1,4,4058410,434160))
//    ((6,4,36),(34,56,59,186,28,1,4,4057240,440640))
//    ((6,5,33),(18,63,19,190,31,6,3,3096690,443880))
//    ((6,5,33),(18,44,90,193,17,6,3,3083190,450720))
    test("optimize strategy4 middleWithZero getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.middleWithZero(_))
    }

//    ((6,4,36),(34,74,59,171,31,1,4,4058410,434160))
//    ((6,4,36),(34,56,59,186,28,1,4,4057240,440640))
//    ((6,5,33),(18,63,19,190,31,6,3,3096690,443880))
    test("optimize strategy4 middleWithLeast getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.middleWithLeast(_))
    }

//    ((6,3,32),(12,25,29,215,31,3,3,3078090,459720))
//    ((6,3,35),(90,38,40,190,28,3,3,3072120,426960))
//    ((6,3,36),(90,38,40,193,27,3,3,3071490,426960))
    test("optimize strategy4 trueMiddleWithZero getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.trueMiddleWithZero(_))
    }

//    ((6,3,32),(12,25,29,215,31,3,3,3078090,459720))
//    ((6,3,35),(90,38,40,190,28,3,3,3072120,426960))
//    ((6,3,33),(90,38,40,207,27,3,3,3073170,426960))
    test("optimize strategy4 trueMiddleWithLeast getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.trueMiddleWithLeast(_))
    }

//    ((6,4,35),(13,39,40,190,38,5,2,2096420,454320))
//    ((6,5,36),(13,39,40,195,35,5,2,2094050,454320))
    test("optimize strategy4 zeroWithLeast getCombinedBetIntersectionStatistics")
    {
        testStrategy4CombinedBetMetrics(_.zeroWithLeast(_))
    }

    def testStrategy4CombinedBetMetrics(
        metric: (Strategy4 with CombinedBetFigureMetrics, Array[RunResult]) => Array[Int]) =
    {
        var result = List[((Int, Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 5; endFigure <- 32 to 36)
        {
            println((betSize, startFigure, endFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, endFigure) with CombinedBetFigureMetrics
            def test = testStrategy[Array[RunResult], Strategy4 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure, endFigure), result, test(metric)(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }
}
