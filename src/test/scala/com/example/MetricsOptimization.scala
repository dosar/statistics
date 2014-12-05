package com.example

import com.example.loto.{Metrics, RunResults}
import org.scalatest.FunSuite

/**
 * Created by alespuh on 05.12.14.
 */
class MetricsOptimization extends FunSuite
{
    test("optimize strategy2")
    {
        val pRange = 1 to 10
        val sRange = 0 to 10
        val fRange = 1 to 11
        val metrics = new Metrics(RunResults.runResults)
        val results = for(pw <- pRange; sw <- sRange; fw <- fRange) yield
        {
            if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
            val result = metrics.strategy2(pw, sw, fw)(metrics.figuresOccurencies)(metrics.topNonZeroFiguresWithoutPrevious)
            (pw, sw, fw, result.count(_._2._1 == 5))
        }

        results.toList.sortBy(- _._4).take(100) foreach println
    }

    test("optimize strategy1")
    {
        val pRange = 1 to 100
        val sRange = 0 to 100
        val fRange = 1 to 100
        val metrics = new Metrics(RunResults.runResults)
        val results = for(pw <- pRange.par; sw <- sRange.par; fw <- fRange) yield
        {
            if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
            (pw, sw, fw, metrics.strategy1(pw, sw, fw)(metrics.topNonZeroFigures).count(_._2._1 == 5))
        }

        results.toList.sortBy(- _._4).take(100) foreach println
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
