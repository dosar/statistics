package com.example.strategy4optimization

import com.example.StrategyOptimizationBase
import com.example.loto.Strategy4
import com.example.loto.metrics.ExceptNonPopularFiguresMetrics
import com.example.loto.model.{RunResult, RunResults}
import org.scalatest.FunSuite

class Strategy4WithExceptNonPopularFiguresOptimization extends FunSuite with StrategyOptimizationBase
{
//    ((6,13,1,36),(13,76,66,192,42,7,2,2115020,220500))
//    ((6,14,1,36),(11,87,68,202,27,6,2,2094170,218880))
    test("optimize strategy4 topExceptIgnored")
    {
        testStrategy4WithExceptNonPopularFiguresMetrics(_.topExceptIgnored(_))
    }

//    ((6,14,1,36),(9,30,41,218,44,4,2,2098520,229500))
//    ((6,16,1,36),(12,18,37,216,24,4,2,2078480,231120))
    test("optimize strategy4 zeroExceptIgnored")
    {
        testStrategy4WithExceptNonPopularFiguresMetrics(_.zeroExceptIgnored(_))
    }

//    ((6,12,1,36),(12,23,70,212,38,5,3,3099060,230220))
//    ((6,14,1,36),(11,24,70,224,34,5,3,3096540,230220))
//    ((6,8,1,36),(11,24,70,219,31,3,3,3078570,230220))
    test("optimize strategy4 leastPopularExceptIgnored")
    {
        testStrategy4WithExceptNonPopularFiguresMetrics(_.leastPopularExceptIgnored(_))
    }

//    ((6,12,1,36),(45,14,51,201,34,4,4,4086580,225900))
//    ((6,13,1,36),(30,8,47,196,41,4,3,3092910,229680))
//    ((6,13,1,36),(40,16,55,239,40,4,3,3097080,226440))
    test("optimize strategy4 middleExceptIgnored")
    {
        testStrategy4WithExceptNonPopularFiguresMetrics(_.middleExceptIgnored(_))
    }

//    ((6,6,1,36),(23,5,68,208,28,3,3,3074280,231480))
//    ((6,1,1,36),(38,47,38,188,34,2,3,3070620,221220))
    test("optimize strategy4 trueMiddleExceptIgnored")
    {
        testStrategy4WithExceptNonPopularFiguresMetrics(_.trueMiddleExceptIgnored(_))
    }

    def  testStrategy4WithExceptNonPopularFiguresMetrics(
        metric: (Strategy4 with ExceptNonPopularFiguresMetrics, Array[RunResult]) => Array[Int]) =
    {
        var result = List[((Int, Int), Statistics)]()
//        val nonPopularFigures = Array(36, 30, 35, 27, 4, 32, 6, 15, 26, 33, 1, 10, 19, 22, 2, 28, 7, 18, 20, 31, 8, 16,
//            25, 23, 13, 24, 5, 21, 3, 34, 17, 29, 14, 12, 9, 11).reverse
        //статистика по всем тиражам до 2338
        val nonPopularFigures = List(34, 14, 13, 29, 12, 17, 18, 3, 7, 21, 5, 16, 25, 1, 11, 15, 9, 19, 32, 24, 4, 2,
            20, 33, 26, 6, 28, 22, 23, 8, 30, 31, 35, 10, 27, 36).toArray
        val rrs = RunResults.runResults
        for(betSize <- 6 to 6; ignoredSize <- 1 to 12)
        {
            println((betSize, ignoredSize))
            val strategy = new Strategy4(rrs, betSize, 1, 36) with ExceptNonPopularFiguresMetrics
            {
                override val figureToIgnores: Array[Figure] = nonPopularFigures.take(ignoredSize)
            }
            def test = testStrategy[Array[RunResult], Strategy4 with ExceptNonPopularFiguresMetrics](strategy, 25)(metric) _
            result = accumulateResult((betSize, ignoredSize), result,
                test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }
}
