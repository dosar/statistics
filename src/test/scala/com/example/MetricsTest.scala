package com.example

import java.util.Date

import com.example.loto.{Metrics, RunResult}
import org.scalatest._

class MetricsTest extends FunSuite
{
    val runResults = Seq(
        RunResult("2189", "30.11.2014 23:59", "15  25  12  26  3"),
        RunResult("2188", "30.11.2014 11:59", "7  13  1  27  14"),
        RunResult("2187", "30.11.2014 23:59", "15  25  12  26  3"),
        RunResult("2186", "29.11.2014 23:59", "34  28  29  2  14"),
        RunResult("2185", "29.11.2014 11:59", "22  33  9  29  5"),
        RunResult("2184", "28.11.2014 23:59", "12  25  13  19  21"),
        RunResult("2183", "28.11.2014 11:59", "30  1  12  28  7"),
        RunResult("2182", "27.11.2014 23:59", "13  25  7  5  10"),
        RunResult("2181", "27.11.2014 11:59", "20  25  30  18  36"),
        RunResult("2180", "26.11.2014 23:59", "1  32  28  21  35"),
        RunResult("2179", "26.11.2014 11:59", "17  6  35  12  14"),
        RunResult("2178", "25.11.2014 23:59", "8  36  2  6  25"),
        RunResult("2177", "25.11.2014 11:59", "30  15  3  36  24"),
        RunResult("2176", "24.11.2014 23:59", "11  5  18  6  33"),
        RunResult("2175", "24.11.2014 11:59", "6  14  18  11  2"),
        RunResult("2174", "23.11.2014 23:59", "30  17  25  14  11"),
        RunResult("2173", "23.11.2014 23:59", "30  17  25  14  11"),
        RunResult("2172", "23.11.2014 11:59", "20  28  33  16  10"),
        RunResult("2171", "22.11.2014 23:59", "2  25  10  20  11"),
        RunResult("2170", "22.11.2014 11:59", "9  4  20  11  3")
    )

    implicit def rr(result: (Int, Int, Int, Int, Int)) =
        RunResult(0, new Date(), result.productIterator.map(_.asInstanceOf[Int]).toArray)

    test("figuresOccurences 1, 2 input elements")
    {
        val firstTenFO = Map(5 -> 1, 10 -> 1, 24 -> 0, 25 -> 1, 14 -> 1,
            20 -> 1, 29 -> 1, 1 -> 1, 6 -> 0, 28 -> 1, 21 -> 1, 33 -> 1, 9 -> 1, 13 -> 1, 2 -> 1, 32 -> 1, 34 -> 1,
            17 -> 0, 22 -> 1, 27 -> 1, 12 -> 1, 7 -> 1, 3 -> 1, 35 -> 1, 18 -> 1, 16 -> 0, 31 -> 0, 11 -> 0, 26 -> 1,
            23 -> 0, 8 -> 0, 36 -> 1, 30 -> 1, 19 -> 1, 4 -> 0, 15 -> 1)
        val firstFO = (1 to 36).map((_, 0)).toMap.updated(15, 1).updated(25, 1).updated(12, 1).updated(26, 1).updated(3, 1)
        assert(new Metrics(runResults.take(1)).allFigureOccurencies === firstFO)
        val secondFO = firstFO.updated(15, 2).updated(25, 2).updated(12, 2).updated(26, 2).updated(3, 2)
        assert(new Metrics(Seq(runResults(0), runResults(2))).allFigureOccurencies === secondFO)
    }

    test("graficData2 past interval 3 future interval 2 takeCount 5")
    {
        check(new Metrics(runResults.take(6), 5).graficData2(3, 2), Seq((Seq(15, 25, 12, 26, 3), 0), (Seq(14, 25, 29, 1, 28), 1)))
    }

    test("graficData4 3 2")
    {
        check(new Metrics(Seq(
            (1, 2, 3, 4, 5),
            (2, 6, 7, 8, 9),
            (1, 3, 5, 7, 9),
            (10, 11, 1, 2, 3),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35)
        ), 5).graficData4(3, 2), Seq((Seq(10, 24, 25, 14, 20), 1), (Seq(24, 25, 14, 20, 29), 1)))
    }

    test("graficData5 3 2")
    {
        check(new Metrics(Seq(
            (1, 2, 3, 4, 5),
            (6, 7, 8, 9, 10),
            (6, 7, 8, 9, 10),
            (12, 11, 1, 5, 3),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35)
        ), 5).graficData5(3, 2), Seq((Seq(1, 2, 3, 4, 5), 3), (Seq(12, 11, 1, 5, 3), 1)))
    }

    test("strategy1 3, 0, 2")
    {
        check(new Metrics(Seq(
            (1, 2, 3, 4, 5),
            (6, 7, 8, 9, 10),
            (6, 7, 8, 9, 10),
            (12, 11, 1, 5, 3),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35)
        ), 5).strategy1(3, 0, 2), Seq((Seq(6, 7, 8, 9, 10), 0)))
    }

    test("strategy1 3, 0, 2")
    {
        check(new Metrics(Seq(
            (6, 7, 8, 9, 10),
            (6, 7, 8, 9, 10),
            (1, 2, 3, 4, 5),
            (6, 7, 1, 5, 3),
            (12, 13, 14, 15, 16),
            (20, 13, 11, 9, 35)
        ), 5).strategy1(2, 0, 2), Seq((Seq(6, 7, 8, 9, 10), 2), (Seq(1, 3, 2, 4, 5), 0)))
    }

    private def check(left: Seq[(Seq[Int], Int)], right: Seq[(Seq[Int], Int)]) =
    {
        def toUnordered(seq: Seq[(Seq[Int], Int)]) = seq.map{case (innerSeq, count) => (innerSeq.toSet, count)}
        assert(toUnordered(left) === toUnordered(right))
    }

    val figures = Map(5 -> 1, 10 -> 1, 25 -> 1, 14 -> 1, 20 -> 1, 29 -> 1, 1 -> 1, 28 -> 1, 21 -> 1, 33 -> 1, 9 -> 1,
        13 -> 1, 2 -> 1, 32 -> 1, 34 -> 1, 22 -> 1, 27 -> 1, 12 -> 1, 7 -> 1, 3 -> 1, 35 -> 1, 18 -> 1, 26 -> 1,
        36 -> 1, 30 -> 1, 19 -> 1, 15 -> 1)
}