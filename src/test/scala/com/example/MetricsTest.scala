package com.example

import com.example.loto.{Metrics, RunResult}
import org.scalatest._

class MetricsTest extends FlatSpec with Matchers
{
    val runResults = Seq(RunResult("2189", "30.11.2014 23:59", "15  25  12  26  3"),
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

    "figuresOccurences correct work" should "work properly" in
    {
        val firstTenFO = Map(5 -> 1, 10 -> 1, 24 -> 0, 25 -> 1, 14 -> 1,
            20 -> 1, 29 -> 1, 1 -> 1, 6 -> 0, 28 -> 1, 21 -> 1, 33 -> 1, 9 -> 1, 13 -> 1, 2 -> 1, 32 -> 1, 34 -> 1,
            17 -> 0, 22 -> 1, 27 -> 1, 12 -> 1, 7 -> 1, 3 -> 1, 35 -> 1, 18 -> 1, 16 -> 0, 31 -> 0, 11 -> 0, 26 -> 1,
            23 -> 0, 8 -> 0, 36 -> 1, 30 -> 1, 19 -> 1, 4 -> 0, 15 -> 1)
        val firstFO = (1 to 36).map((_, 0)).toMap.updated(15, 1).updated(25, 1).updated(12, 1).updated(26, 1).updated(3, 1)
        Seq(new Metrics(runResults.take(1)).allFigureOccurencies.toList) should contain (firstFO.toList)
        val secondFO = firstFO.updated(15, 2).updated(25, 2).updated(12, 2).updated(26, 2).updated(3, 2)
        Seq(new Metrics(Seq(runResults(0), runResults(2))).allFigureOccurencies.toList) should contain (secondFO.toList)
    }

    "figuresOccurences" should "show frequence" in
    {
        val firstTenFO = Map(5 -> 1, 10 -> 1, 24 -> 0, 25 -> 1, 14 -> 1,
            20 -> 1, 29 -> 1, 1 -> 1, 6 -> 0, 28 -> 1, 21 -> 1, 33 -> 1, 9 -> 1, 13 -> 1, 2 -> 1, 32 -> 1, 34 -> 1,
            17 -> 0, 22 -> 1, 27 -> 1, 12 -> 1, 7 -> 1, 3 -> 1, 35 -> 1, 18 -> 1, 16 -> 0, 31 -> 0, 11 -> 0, 26 -> 1,
            23 -> 0, 8 -> 0, 36 -> 1, 30 -> 1, 19 -> 1, 4 -> 0, 15 -> 1)
        new Metrics(runResults.take(10)).allFigureOccurencies should be (firstTenFO)
        new Metrics(runResults.drop(1).take(10)).allFigureOccurencies should not be (firstTenFO)
    }

    "graficData2 10 7" should "produce the correct result" in
    {
        new Metrics(runResults).graficData2(10, 7) should be (Seq((Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),2),
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),2),
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),3),
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),3)))
    }

    "graficData2 5 10" should "produce the correct result" in
    {
        new Metrics(runResults).graficData2(5, 10) should be (Seq(
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),3),
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),3),
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),3),
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),2),
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),3),
            (Seq(5, 10, 24, 25, 14, 20, 29, 1, 6),3)))
    }

    val figures = Map(5 -> 1, 10 -> 1, 25 -> 1, 14 -> 1, 20 -> 1, 29 -> 1, 1 -> 1, 28 -> 1, 21 -> 1, 33 -> 1, 9 -> 1,
        13 -> 1, 2 -> 1, 32 -> 1, 34 -> 1, 22 -> 1, 27 -> 1, 12 -> 1, 7 -> 1, 3 -> 1, 35 -> 1, 18 -> 1, 26 -> 1,
        36 -> 1, 30 -> 1, 19 -> 1, 15 -> 1)
}
