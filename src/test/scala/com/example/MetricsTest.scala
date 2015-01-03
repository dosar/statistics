package com.example

import com.example.loto.model.{RunResults, RunResult}
import com.example.loto._

import scala.collection.mutable

class MetricsTest extends TestBase
{
    val runResults = Vector(
        RunResult("2189", "30.11.2014 23:59", "15  25  12  26  3"),
        RunResult("2188", "30.11.2014 11:59", "7   13  1   27  14"),
        RunResult("2187", "30.11.2014 23:59", "15  25  12  26  3"),
        RunResult("2186", "29.11.2014 23:59", "34  28  29  2   14"),
        RunResult("2185", "29.11.2014 11:59", "22  33  9   29  5"),
        RunResult("2184", "28.11.2014 23:59", "12  25  13  19  21"),
        RunResult("2183", "28.11.2014 11:59", "30  1   12  28  7"),
        RunResult("2182", "27.11.2014 23:59", "13  25  7   5   10"),
        RunResult("2181", "27.11.2014 11:59", "20  25  30  18  36"),
        RunResult("2180", "26.11.2014 23:59", "1   32  28  21  35"),
        RunResult("2179", "26.11.2014 11:59", "17  6   35  12  14"),
        RunResult("2178", "25.11.2014 23:59", "8   36  2   6   25"),
        RunResult("2177", "25.11.2014 11:59", "30  15  3   36  24"),
        RunResult("2176", "24.11.2014 23:59", "11  5   18  6   33"),
        RunResult("2175", "24.11.2014 11:59", "6   14  18  11  2"),
        RunResult("2174", "23.11.2014 23:59", "30  17  25  14  11"),
        RunResult("2173", "23.11.2014 23:59", "30  17  25  14  11"),
        RunResult("2172", "23.11.2014 11:59", "20  28  33  16  10"),
        RunResult("2171", "22.11.2014 23:59", "2   25  10  20  11"),
        RunResult("2170", "22.11.2014 11:59", "9   4   20  11  3")
    )

    test("figureOccurencies on all data")
    {
        val metrics = new Metrics()
        FiguresByHitSorter.topFigures(metrics.figuresOccurencies(RunResults.runResults, 1, 36)) foreach println
    }

    test("pair occurencies on all data")
    {
        val map = mutable.Map[(Int, Int), Int]() withDefaultValue(0)
        for (rr <- RunResults.runResults)
        {
            for(i <- 0 until rr.result.length - 1; j <- i + 1 until rr.result.length)
                map((rr.result(i), rr.result(j))) += 1
        }
        map.toVector.sortBy(_._2) foreach println
    }

    test("trustedIntervals")
    {
        val metrics = new Metrics()
        assert(metrics.figuresTrustedInterval(Vector(1, 2, 3, 4, 5), 1.0) === (1, 5))
        assert(metrics.figuresTrustedInterval(Vector(1, 2, 3, 4, 5), 0.9) === (2, 5))
        assert(metrics.figuresTrustedInterval(Vector(1, 2, 3, 4, 5), 0.8) === (2, 5))
        assert(metrics.figuresTrustedInterval(Vector(1, 1, 2, 3, 4, 5), 0.75) === (2, 4))
        assert(metrics.figuresTrustedInterval(Vector(1, 1, 1, 2, 2, 3, 4, 5, 5, 5, 5), 0.5) === (2, 4))
    }

    test("figuresOccurences 1, 2 input elements")
    {
        assert(new SimpleGraphics(runResults.take(1)).allFigureOccurencies.zipWithIndex.filter(_._1 > 0).map(_._2).toSet ===
            Set(15, 25, 12, 26, 3))
        assert(new SimpleGraphics(Vector(runResults(0), runResults(2))).allFigureOccurencies.zipWithIndex.filter(_._1 > 1).map(_._2).toSet ===
            Set(15, 25, 12, 26, 3))
    }

    test("graficData2 past interval 3 future interval 2 takeCount 5")
    {
        check(new SimpleGraphics(runResults.take(6), 5).graficData2(3, 2), Vector((Seq(15, 25, 12, 26, 3), 0), (Seq(14, 29, 28, 34, 27), 1)))
    }

    test("graficData4 3 2")
    {
        check(new SimpleGraphics(Vector(
            (1, 2, 3, 4, 5),
            (2, 6, 7, 8, 9),
            (1, 3, 5, 7, 9),
            (10, 11, 1, 2, 3),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35)
        ), 5, 17, 36).graficData4(3, 2), Vector((Seq(17, 18, 19, 20, 21),0), (Seq(17, 18, 19, 20, 21),1)))
    }

    test("graficData5 3 2")
    {
        check(new SimpleGraphics(Vector(
            (1, 2, 3, 4, 5),
            (6, 7, 8, 9, 10),
            (6, 7, 8, 9, 10),
            (12, 11, 1, 5, 3),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35)
        ), 5).graficData5(3, 2), Seq((Seq(1, 2, 3, 4, 5), 3), (Seq(12, 11, 1, 5, 3), 1)))
    }

    test("topNonZeroFiguresWithoutPrevious")
    {
        val metrics = new MetricsTypes{ val topFiguresCount = 5 }
        assert(metrics.topNonZeroFiguresWithoutPrevious(Array(6 -> 2, 3 -> 1, 2 -> 1, 10 -> 1), (6, 7, 8, 9, 10)) === Array(3, 2, 0, 0, 0))
    }

    test("getIntersections")
    {
        val strategy = new Strategy2(Vector((6, 7, 8, 9, 10)), 5)
        assert(strategy.getIntersections(Vector((rr(6, 3, 2, 10, 1), 1)),
            Array(6 -> 2, 3 -> 1, 2 -> 1, 10 -> 1))(strategy.topNonZeroFiguresWithoutPrevious) === (2, 1))
    }

    val figures = Map(5 -> 1, 10 -> 1, 25 -> 1, 14 -> 1, 20 -> 1, 29 -> 1, 1 -> 1, 28 -> 1, 21 -> 1, 33 -> 1, 9 -> 1,
        13 -> 1, 2 -> 1, 32 -> 1, 34 -> 1, 22 -> 1, 27 -> 1, 12 -> 1, 7 -> 1, 3 -> 1, 35 -> 1, 18 -> 1, 26 -> 1,
        36 -> 1, 30 -> 1, 19 -> 1, 15 -> 1)
}