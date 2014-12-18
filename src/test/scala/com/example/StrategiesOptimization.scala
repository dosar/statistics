package com.example

import com.example.loto.model.{RunResult, RunResults}
import com.example.loto._
import org.scalatest.FunSuite

import scala.collection.immutable.IndexedSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class StrategiesOptimization extends FunSuite
{
    test("optimize strategy5 without non popular figures")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 6 to 8; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36)
            result = result ++ testStrategy4(strategy, 50)(_.topNonZeroFiguresWithoutNotPopular(_))
                .map(x => ((betSize, startFigure), x))
            result = result.sortBy(x => x._2._9 - x._2._8).take(200)
        }
        result foreach println
    }

    test("optimize strategy4 with middle popular figures")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 7 to 8; startFigure <- 17 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36)
            result = result ++ testStrategy4(strategy, 50)(_.middleOccurencyFigures(_))
                .map(x => ((betSize, startFigure), x))
            result = result.sortBy(x => x._2._9 - x._2._8).take(200)
        }
        result foreach println
    }

    test("optimize strategy4 with zero popular figures")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 6 to 9; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36)
            result = result ++ testStrategy4(strategy, 50)(_.zeroOccurencyFigures(_))
                .map(x => ((betSize, startFigure), x))
            result = result.sortBy(x => x._2._9 - x._2._8).take(200)
        }
        result foreach println
    }

    test("optimize strategy4 without non popular figures")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 7 to 8; startFigure <- 17 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36)
            result = result ++ testStrategy4(strategy, 50)(_.topNonZeroFiguresWithoutNotPopular(_))
                .map(x => ((betSize, startFigure), x))
            result = result.sortBy(x => x._2._9 - x._2._8).take(200)
        }
        result foreach println
    }

/*
    test("optimize strategy3 without non popular figures")
    {
        var result = List[(Int, Int, Int, Int)]()
        for(startFigure <- 1 to 17; endFigure <- 17 to 36 if endFigure - startFigure > 7)
        {
            println((startFigure, endFigure))
            result = result ++ testStrategy3(7, startFigure, endFigure)((s, rrs) => s.topNonZeroFiguresWithoutNotPopular(rrs))
            result = result.sortBy(- _._4).take(200)
        }
        result foreach println
    }

    test("optimize strategy2")
    {
        testStrategy2()((m, rrs) => m.figuresOccurencies(rrs).toArray.sortBy(- _._2))
    }

    test("optimize strategy2 without non popular figures")
    {
        testStrategy2(17)((m, rrs) => m.figuresOccurencies(rrs).toArray.sortBy(- _._2))
    }

    test("optimize strategy1")
    {
        testStrategy1((m, rrs) => m.topNonZeroFigures(rrs))
    }

    test("optimize strategy1 without NotPopularFigures")
    {
        testStrategy1((m, rrs) => m.topNonZeroFiguresWithoutNotPopular(rrs))
    }

*/
    def testStrategy1(betGenerator: (Strategy1, Vector[RunResult]) => Array[Int]): Unit =
        testStrategy(new Strategy1(RunResults.runResults), betGenerator)

    def testStrategy3(topFiguresCount: Int, startFigure: Int, endFigure: Int)(betGenerator: (Strategy3, Vector[RunResult]) => Array[Int]) =
        testStrategy(new Strategy3(RunResults.runResults, topFiguresCount, startFigure, endFigure), betGenerator)

    def testStrategy2(startFigure: Int = 1)(extractor: (Strategy2, Vector[RunResult]) => Array[(Int, Int)]): Unit =
    {
        val strategy = new Strategy2(RunResults.runResults, startFigure = startFigure)

        def figuresOccurencies(rrs: Vector[RunResult]) =
            strategy.figuresOccurencies(rrs).toArray.sortBy(- _._2)

        val pRange = 1 to 300
        val sRange = 0 to 300
        val fRange = 1 to 300
        val results = for(pw <- pRange.par; sw <- sRange.par; fw <- fRange.par) yield
        {
            if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
            val result = strategy.strategy2(pw, sw, fw)(rrs => extractor(strategy, rrs))(strategy.topNonZeroFiguresWithoutPrevious)
            (pw, sw, fw, result.count(_._2._1 == 5))
        }

        results.toList.sortBy(- _._4).take(200) foreach println
    }

    type Strategy =
    {
        def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(
            betGenerator: Vector[RunResult] => Array[Int]): Seq[(Array[Int], (Int, Int))]
    }

    def testStrategy[TStrategy <: Strategy](strategy: TStrategy, betGenerator: (TStrategy, Vector[RunResult]) => Array[Int]): IndexedSeq[(Int, Int, Int, Int)] =
    {
        val pRangeSize = 25
        val pRange1 = 1 to pRangeSize
        val pRange2 = (pRangeSize + 1) to 2 * pRangeSize
        val pRange3 = (2 * pRangeSize + 1) to 3 * pRangeSize
        val pRange4 = (3 * pRangeSize + 1) to 4 * pRangeSize
        val sRange = 0 to 100
        val fRange = 1 to 100

        val sorter = new SimpleParallelSort[(Int, Int, Int, Int)](4, 50, (0, 0, 0, 0))(_._4)

        def calcFragment(pRange: Range, fragment: Int) =
        {
            for(pw <- pRange; sw <- sRange; fw <- fRange)
            {
                if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
                sorter.update(fragment,
                    (pw, sw, fw, strategy(pw, sw, fw)(rrs => betGenerator(strategy, rrs)).count(_._2._1 == 5)))
            }
        }

        val future1 = Future { calcFragment(pRange1, 0) }
        val future2 = Future { calcFragment(pRange2, 1) }
        val future3 = Future { calcFragment(pRange3, 2) }
        val future4 = Future { calcFragment(pRange4, 3) }
        val result = for{result1 <- future1; result2 <- future2; result3 <- future3; result4 <- future4} yield
        {
            sorter.result
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

    def testStrategy4[TStrategy <: StrategyWithMoneyStatistics](strategy: TStrategy, chunkSize: Int)(
        betGenerator: (TStrategy, Vector[RunResult]) => Array[Int]) =
    {
        val pRangeSize = chunkSize
        val pRange1 = 1 to pRangeSize
        val pRange2 = (pRangeSize + 1) to 2 * pRangeSize
        val pRange3 = (2 * pRangeSize + 1) to 3 * pRangeSize
        val pRange4 = (3 * pRangeSize + 1) to 4 * pRangeSize
        val sRange = 0 to pRangeSize * 4
        val fRange = 1 to pRangeSize * 4

        type PastWindow = Int; type SkipWindow = Int; type FutureWindow = Int
        type Hit2 = Int; type Hit3 = Int; type Hit4 = Int; type Hit5 = Int
        type MoneyPlus = Int; type MoneyMinus = Int

        val sorter = new SimpleParallelSort[(PastWindow, SkipWindow, FutureWindow, Hit2, Hit3, Hit4, Hit5,
            MoneyPlus, MoneyMinus)](4, 50, (0, 0, 0, 0, 0, 0, 0, 0, 0))(x => x._8 - x._9,
        { (left: Int, right: Int) => (left / 100000).compareTo(right / 100000) })

        def calcFragment(pRange: Range, fragment: Int) =
        {
            for(pw <- pRange; sw <- sRange; fw <- fRange)
            {
                if(pw % 10 == 0 && sw == 0 && fw == 1) println("+")
                val strategyResult = strategy.apply(pw, sw, fw)(rrs => betGenerator(strategy, rrs))
                if(strategyResult.forall(x => x._2._5 - x._2._6 > -(fw * 1000) /*&& x._2._6 <= 10000*/))
                {
                    val (hit2, hit3, hit4, hit5, mplus, mminus) = strategyResult.foldLeft(0, 0, 0, 0, 0, 0)
                    { case ((ah2, ah3, ah4, ah5, amp, amm), (_, (h2, h3, h4, h5, mp, mm))) =>
                        (ah2 + h2, ah3 + h3, ah4 + h4, ah5 + h5, amp + mp, amm + mm)
                    }
                    sorter.update(fragment, (pw, sw, fw, hit2, hit3, hit4, hit5, mplus, mminus))
                }
            }
        }

        val future1 = Future { calcFragment(pRange1, 0) }
        val future2 = Future { calcFragment(pRange2, 1) }
        val future3 = Future { calcFragment(pRange3, 2) }
        val future4 = Future { calcFragment(pRange4, 3) }
        val result = for{result1 <- future1; result2 <- future2; result3 <- future3; result4 <- future4} yield
        {
            sorter.result
        }
        Await.result(result, 120 minutes)
    }
}
