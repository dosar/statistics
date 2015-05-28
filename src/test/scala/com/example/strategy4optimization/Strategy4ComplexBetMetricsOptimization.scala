package com.example.strategy4optimization

import com.example.StrategyOptimizationBase
import com.example.loto.Strategy4
import com.example.loto.metrics.ComplexBetFigureMetrics
import com.example.loto.model.{RunResult, RunResults}
import org.scalatest.FunSuite

/**
 * Created by alespuh on 31.01.15.
 */
class Strategy4ComplexBetMetricsOptimization extends FunSuite with StrategyOptimizationBase
{
//    ((6,4,36,2),(34,74,59,171,31,1,4,4058410,217080))
//    ((6,4,36,2),(34,56,59,186,28,1,4,4057240,220320))
    test("optimize strategy4 complex bet middleAndTrueMiddle")
    {
        testStrategy4ComplexBetMetrics(_.middleAndTrueMiddle(_))
    }

//    ((6,4,35,4),(9,66,65,204,20,5,3,3080280,223020))
//    ((6,3,33,2),(14,36,19,221,31,4,3,3086010,227520))
//    ((6,3,33,1),(15,34,65,203,30,4,3,3082860,227700))
    test("optimize strategy4 complex bet middleAndLeast")
    {
        testStrategy4ComplexBetMetrics(_.middleAndLeast(_))
    }

//    ((6,3,36,1),(20,64,27,183,29,4,3,3079470,221400))
//    ((6,2,36,2),(79,51,26,198,28,4,3,3080280,213120))
    test("optimize strategy4 complex bet middleAndZero")
    {
        testStrategy4ComplexBetMetrics(_.middleAndZero(_))
    }

//    ((6,2,34,2),(54,17,80,200,41,4,3,3093390,223740))
//    ((6,2,34,2),(54,78,41,177,31,4,3,3080730,212760))
    test("optimize strategy4 complex bet trueMiddleAndLeast")
    {
        testStrategy4ComplexBetMetrics(_.trueMiddleAndLeast(_))
    }

//    ((6,2,34,2),(54,17,80,200,41,4,3,3093390,223740))
//    ((6,2,34,2),(54,78,41,177,31,4,3,3080730,212760))
    test("optimize strategy4 complex bet trueMiddleAndZero")
    {
        testStrategy4ComplexBetMetrics(_.trueMiddleAndZero(_))
    }

    test("optimize strategy4 complex bet leastAndZero")
    {
        testStrategy4ComplexBetMetrics(_.leastAndZero(_))
    }

//    ((6,2,35,5),(52,21,17,207,19,4,3,3072450,223380))
//    ((6,2,33,1),(19,35,92,184,27,3,3,3070410,226800))
    test("optimize strategy4 complex bet topAndMiddle")
    {
        testStrategy4ComplexBetMetrics(_.topAndMiddle(_))
    }

//    ((6,3,34,2),(14,24,58,167,34,2,4,4068100,229680))
//    ((6,1,33,5),(53,83,82,197,26,1,4,4056580,212040))
//    ((6,2,33,3),(13,19,51,196,16,7,3,3089760,230760))
    test("optimize strategy4 complex bet topAndTrueMiddle")
    {
        testStrategy4ComplexBetMetrics(_.topAndTrueMiddle(_))
    }

    //((6,10,4),(22,4,66,198,32,3,3,3077040,229320))
    test("optimize strategy4 complex bet topAndLeastPopular")
    {
        testStrategy4ComplexBetMetrics(_.topAndLeastPopular(_))
    }

    //((6,11,4),(19,83,70,217,30,4,3,3084540,215640))
    //((6,4,35,3),(22,16,45,187,22,3,4,4065820,227160))
    test("optimize strategy4 complex bet topAndZero")
    {
        testStrategy4ComplexBetMetrics(_.topAndZero(_))
    }

    //((6,6,2),(56,66,30,187,39,1,4,4068250,212040))
    //((6,3,32,2),(55,67,32,201,32,2,4,4070200,212040))
    //((6,4,28,3),(33,76,36,213,29,2,4,4068670,214380))
    //((6,10,2),(67,30,76,187,30,7,3,3102540,216540))
    test("optimize strategy4 complex bet topAndMiddlePartiallyFixedBet")
    {
        testStrategy4ComplexBetMetrics(_.topAndMiddlePartiallyFixedBet(_))
    }

    def testStrategy4ComplexBetMetrics(
        metric: (Strategy4 with ComplexBetFigureMetrics, Array[RunResult]) => Array[Int]) =
    {
        var result = List[((Int, Int, Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 4; endFigure <- 33 to 36; betPart2Size <- 1 to 5)
        {
            println((betSize, startFigure, endFigure, betPart2Size))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, endFigure) with ComplexBetFigureMetrics
            {
                override val endSize = betPart2Size
            }
            def test = testStrategy[Array[RunResult], Strategy4 with ComplexBetFigureMetrics](strategy, 25) _
            result = accumulateResult((betSize, startFigure, endFigure, betPart2Size), result, test(metric)(strategy.getIntersectionStatistics))
        }
        result foreach println
    }
}
