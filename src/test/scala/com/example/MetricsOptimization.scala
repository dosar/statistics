package com.example

import com.example.loto.{Metrics, RunResult, RunResults, SimpleParallelSort}
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class MetricsOptimization extends FunSuite
{
    test("optimize strategy2")
    {
        testStrategy2((m, rrs) => m.figuresOccurencies(rrs).toArray.sortBy(- _._2))
    }

    test("optimize strategy2 without non popular figures")
    {
        testStrategy2((m, rrs) => m.figuresOccurencies(rrs, 17).toArray.sortBy(- _._2))
    }

    test("optimize strategy1")
    {
        testStrategy1((m, rrs) => m.topNonZeroFigures(rrs))
    }

    test("optimize strategy1 without NotPopularFigures")
    {
        testStrategy1((m, rrs) => m.topNonZeroFiguresWithoutNotPopular(rrs))
    }

    def testStrategy2(extractor: (Metrics, Seq[RunResult]) => Array[(Int, Int)]): Unit =
    {
        val metrics = new Metrics(RunResults.runResults)

        def figuresOccurencies(rrs: Seq[RunResult]) =
            metrics.figuresOccurencies(rrs).toArray.sortBy(- _._2)

        val pRange = 1 to 300
        val sRange = 0 to 300
        val fRange = 1 to 300
        val results = for(pw <- pRange.par; sw <- sRange.par; fw <- fRange.par) yield
        {
            if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
            val result = metrics.strategy2(pw, sw, fw)(rrs => extractor(metrics, rrs))(metrics.topNonZeroFiguresWithoutPrevious)
            (pw, sw, fw, result.count(_._2._1 == 5))
        }

        results.toList.sortBy(- _._4).take(200) foreach println
    }

    def testStrategy1(betGenerator: (Metrics, Seq[RunResult]) => Array[Int]): Unit =
    {
        val metrics = new Metrics(RunResults.runResults)
        val pRangeSize = 75
        val pRange1 = 1 to pRangeSize
        val pRange2 = (pRangeSize + 1) to 2 * pRangeSize
        val pRange3 = (2 * pRangeSize + 1) to 3 * pRangeSize
        val pRange4 = (3 * pRangeSize + 1) to 4 * pRangeSize
        val sRange = 0 to 300
        val fRange = 1 to 300

        val sorter = new SimpleParallelSort[(Int, Int, Int, Int)](4, 50, (0, 0, 0, 0))(_._4)

        def calcFragment(pRange: Range, fragment: Int) =
        {
            for(pw <- pRange; sw <- sRange; fw <- fRange)
            {
                if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
                sorter.update(fragment,
                    (pw, sw, fw, metrics.strategy1(pw, sw, fw)(rrs => betGenerator(metrics, rrs)).count(_._2._1 == 5)))
            }
        }

        val future1 = Future { calcFragment(pRange1, 0) }
        val future2 = Future { calcFragment(pRange2, 1) }
        val future3 = Future { calcFragment(pRange3, 2) }
        val future4 = Future { calcFragment(pRange4, 3) }
        val result = for{result1 <- future1; result2 <- future2; result3 <- future3; result4 <- future4} yield
        {
            sorter.result foreach println
        }
        Await.result(result, 120 minutes)

    }

    test("sortBy on large vectors")
    {
        val result = new Array[(Int, Int, Int, Int)](27000000)
        var ind = 0
        while(ind < result.length)
        {
            if(ind % 1000000 == 0) println("+")
            result(ind) = (Random.nextInt(), Random.nextInt(), Random.nextInt(), Random.nextInt())
            ind += 1
        }
        result.sortBy(- _._4).take(200) foreach println
    }

/*
    test("optimized optimize strategy1")
    {
        val pwiLowerBound = 1
        val pwiUpperBound = 100
        var pwi = pwiLowerBound

        val swiLowerBound = 0
        val swiUpperBound = 100
        var swi = swiLowerBound

        val fwiLowerBound = 1
        val fwiUpperBound = 100
        var fwi = fwiLowerBound

        val metrics = new Metrics(RunResults.runResults)
        var results = ArrayBuffer[(Int, Int, Int, Int)]()
        while(pwi <= pwiUpperBound)
        {
            if(pwi % 10 == 0) println("+")
            swi = swiLowerBound
            while(swi <= swiUpperBound)
            {
                fwi = fwiLowerBound
                while(fwi <= fwiUpperBound)
                {
                    results += ((pwi, swi, fwi, metrics.strategy1(pwi, swi, fwi)(metrics.topNonZeroFigures).count(_._2._1 == 5)))
                    fwi += 1
                }
                swi += 1
            }
            pwi += 1
        }
        results.toList.sortBy(- _._4).take(100) foreach println
    }
*/
}
