package com.example

import com.example.loto._
import com.example.loto.model.{RunResult, RunResults}
import org.scalatest.FunSuite

import scala.collection.immutable.IndexedSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class StrategiesOptimization extends FunSuite
{
    type PastWindow = Int; type SkipWindow = Int; type FutureWindow = Int
    type Hit2 = Int; type Hit3 = Int; type Hit4 = Int; type Hit5 = Int
    type MoneyPlus = Int; type MoneyMinus = Int
    type Statistics = (PastWindow, SkipWindow, FutureWindow, Hit2, Hit3, Hit4, Hit5, MoneyPlus, MoneyMinus)

    def accumulateResult(betSize: Int, startFigure: Int, result: List[((Int, Int), Statistics)], append: Seq[Statistics]) =
        (result ++ append.map(x => ((betSize, startFigure), x))).sortBy{ case (_, x) => (x._7, x._6, x._5) }.reverse.take(200)

    test("optimize strategy4 probabalisticIntervalFigures getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with ProbabalisticIntervalsMetrics
            {
                override val probabilities: Array[Double] = Array(1.0, 1.0, 1.0, 1.0, 1.0)
            }
            def test = testStrategy4[Array[RunResult], Strategy4 with ProbabalisticIntervalsMetrics](strategy, 10) _
            result = accumulateResult(betSize, startFigure, result, test(_.probabalisticIntervalFigures(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    /*тоже есть 4 хита в 5ку*/
    test("optimize strategy4 topWithMiddle getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy4[Array[RunResult], Strategy4 with CombinedBetFigureMetrics](strategy, 50) _
            result = accumulateResult(betSize, startFigure, result, test(_.topWithMiddle(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    /*3 хита*/
    test("optimize strategy4 topWithTrueMiddle getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy4[Array[RunResult], Strategy4 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult(betSize, startFigure, result, test(_.topWithTrueMiddle(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    /*3 хита*/
    test("optimize strategy4 topWithZero getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy4[Array[RunResult], Strategy4 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult(betSize, startFigure, result, test(_.topWithZero(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy4 middleWithTrueMiddle getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 10 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy4[Array[RunResult], Strategy4 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult(betSize, startFigure, result, test(_.middleWithTrueMiddle(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy4 middleWithZero getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy4[Array[RunResult], Strategy4 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult(betSize, startFigure, result, test(_.middleWithZero(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy4 trueMiddleWithZero getCombinedBetIntersectionStatistics")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with CombinedBetFigureMetrics
            def test = testStrategy4[Array[RunResult], Strategy4 with CombinedBetFigureMetrics](strategy, 25) _
            result = accumulateResult(betSize, startFigure, result, test(_.trueMiddleWithZero(_))(strategy.getCombinedBetIntersectionStatistics))
        }
        result foreach println
    }

    /* хороша по хитам в 4ки*/
    test("optimize strategy4 topPairFigures")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with PairFigureMetrics
            def test = testStrategy4[Array[RunResult], Strategy4 with PairFigureMetrics](strategy, 50)(_.topPairFigures(_)) _
            result = accumulateResult(betSize, startFigure, result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /* тут тоже не очень */
    test("optimize strategy4 fromMiddleOccurencies")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        val rrs = RunResults.runResults
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(rrs, betSize, startFigure, 36)
            def test = testStrategy4[Array[RunResult], Strategy4](strategy, 50)(_.fromMiddleOccurencies(_)) _
            result = accumulateResult(betSize, startFigure, result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /* лучшая стратега на данный момент */
    test("optimize strategy4 middleOccurencyFigures")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36)
            def test = testStrategy4[Array[RunResult], Strategy4](strategy, 50)(_.middleOccurencyFigures(_)) _
            result = accumulateResult(betSize, startFigure, result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /* тут тоже не очень */
    test("optimize strategy4 zeroOccurencyFigures")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36)
            def test = testStrategy4[Array[RunResult], Strategy4](strategy, 50)(_.zeroOccurencyFigures(_)) _
            result = accumulateResult(betSize, startFigure, result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /* эта шляпа стабильно не приносит хороших результатов */
    test("optimize strategy4 topNonZeroFiguresGeneric1")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36)
            def test = testStrategy4[Array[RunResult], Strategy4](strategy, 50)(_.topNonZeroFiguresGeneric1(_)) _
            result = accumulateResult(betSize, startFigure, result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    /* вторая стратегия из лучших */
    test("optimize strategy5 topNonZeroFiguresGeneric")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy5(RunResults.runResults, betSize, startFigure, 36)
            def test = testStrategy4[Array[RunResult], Strategy5](strategy, 50)(_.topNonZeroFiguresGeneric(_)) _
            result = accumulateResult(betSize, startFigure, result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy4 topNonZeroFiguresExceptSome")
    {
        var result = List[((Int, Int), (Int, Int, Int, Int, Int, Int, Int, Int, Int))]()
        val nonPopularFigures = Array(36, 30, 35, 27, 4, 32, 6, 15, 26, 33, 1, 10, 19, 22, 2, 28, 7, 18, 20, 31, 8, 16,
            25, 23, 13, 24, 5, 21, 3, 34, 17, 29, 14, 12, 9, 11).reverse
        val rrs = RunResults.runResults
        for(betSize <- 6 to 6; ignoredSize <- 1 to 17)
        {
            println((betSize, ignoredSize))
            val nonUseful = nonPopularFigures.take(ignoredSize).sorted :+ 0
            val strategy = new Strategy4(rrs, betSize, 1, 36)
            def test = testStrategy4[Array[RunResult], Strategy4](strategy, 50)(_.topNonZeroFiguresExceptSome(_, nonUseful)) _
            result = accumulateResult(betSize, ignoredSize, result, test(strategy.getIntersectionStatistics))
        }
        result foreach println
    }

    test("optimize strategy4 topFigureCombinedWithPair")
    {
        var result = List[((Int, Int), Statistics)]()
        for(betSize <- 6 to 6; startFigure <- 1 to 17)
        {
            println((betSize, startFigure))
            val strategy = new Strategy4(RunResults.runResults, betSize, startFigure, 36) with PairFigureMetrics
            def test = testStrategy4[Array[RunResult], Strategy4 with PairFigureMetrics](strategy, 50) _
            result = accumulateResult(betSize, startFigure, result, test(_.topFigureCombinedWithPair(_))(strategy.getIntersectionStatistics))
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
*/
    def testStrategy1(betGenerator: (Strategy1, Array[RunResult]) => Array[Int]): Unit =
        testStrategy(new Strategy1(RunResults.runResults), betGenerator)

    def testStrategy3(topFiguresCount: Int, startFigure: Int, endFigure: Int)(betGenerator: (Strategy3, Array[RunResult]) => Array[Int]) =
        testStrategy(new Strategy3(RunResults.runResults, topFiguresCount, startFigure, endFigure), betGenerator)

    def testStrategy2(startFigure: Int = 1)(extractor: (Strategy2, Array[RunResult]) => Array[(Int, Int)]): Unit =
    {
        val strategy = new Strategy2(RunResults.runResults, startFigure = startFigure)

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
            betGenerator: Array[RunResult] => Array[Int]): Seq[(Array[Int], (Int, Int))]
    }

    def testStrategy[TStrategy <: Strategy](strategy: TStrategy, betGenerator: (TStrategy, Array[RunResult]) => Array[Int]): IndexedSeq[(Int, Int, Int, Int)] =
    {
        val pRangeSize = 25
        val pRange1 = 1 to pRangeSize
        val pRange2 = (pRangeSize + 1) to 2 * pRangeSize
        val pRange3 = (2 * pRangeSize + 1) to 3 * pRangeSize
        val pRange4 = (3 * pRangeSize + 1) to 4 * pRangeSize
        val sRange = 0 to 100
        val fRange = 1 to 100

        val sorter = new SimpleParallelSort[(Int, Int, Int, Int), Int](4, 50, (0, 0, 0, 0))(_._4)

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
        val awaitedResult = Await.result(result, 120 minutes)
        awaitedResult foreach println
        awaitedResult
    }

    def testStrategy4[TFrom, TStrategy <: StrategyWithMoneyStatistics[TFrom, Array[Int]]](strategy: TStrategy, chunkSize: Int)(
        betGenerator: (TStrategy, TFrom) => Array[Int])(intersectionStatistics: (Array[RunResult], Array[Int]) => (MoneyHitStatisticsType#StrategyStatistics, MoneyHitStatisticsType#SliceSize)): IndexedSeq[Statistics] =
    {
        val pRangeSize = chunkSize
        val pRange1 = 1 to pRangeSize
        val pRange2 = (pRangeSize + 1) to 2 * pRangeSize
        val pRange3 = (2 * pRangeSize + 1) to 3 * pRangeSize
        val pRange4 = (3 * pRangeSize + 1) to 4 * pRangeSize
        val sRange = 0 to pRangeSize * 4
        val fRangeStart = 1
//        val fRangeStart = 10
        val fRange = fRangeStart to pRangeSize * 4
//        val fRange = fRangeStart to 60

//        val sorter = new SimpleParallelSort[(PastWindow, SkipWindow, FutureWindow, Hit2, Hit3, Hit4, Hit5,
//            MoneyPlus, MoneyMinus)](4, 50, (0, 0, 0, 0, 0, 0, 0, 0, 0))(x => x._8 - x._9,
//            { (left: Int, right: Int) => (left / 100000).compareTo(right / 100000) })

        val sorter = new SimpleParallelSort[Statistics, (Hit5, Hit4, Hit3)](4, 50, (0, 0, 0, 0, 0, 0, 0, 0, 0))(x => (x._7, x._6, x._5))

        def calcFragment(pRange: Range, fragment: Int) =
        {
            for(pw <- pRange; sw <- sRange; fw <- fRange)
            {
                if(pw % 10 == 0 && sw == 0 && fw == fRangeStart) println("+")
                val strategyResult = strategy.apply(pw, sw, fw)(rrs => betGenerator(strategy, rrs))(intersectionStatistics)
//                if(strategyResult.forall(x => x._2._5 - x._2._6 > -(fw * 1000) /*&& x._2._6 <= 10000*/))
//                {
                val (hit2, hit3, hit4, hit5, mplus, mminus) = strategyResult.foldLeft(0, 0, 0, 0, 0, 0)
                { case ((ah2, ah3, ah4, ah5, amp, amm), (_, (h2, h3, h4, h5, mp, mm))) =>
                    (ah2 + h2, ah3 + h3, ah4 + h4, ah5 + h5, amp + mp, amm + mm)
                }
                    sorter.update(fragment, (pw, sw, fw, hit2, hit3, hit4, hit5, mplus, mminus))
//                }
            }
        }

        calcFutureResult(sorter, calcFragment(pRange1, 0), calcFragment(pRange2, 1), calcFragment(pRange3, 2), calcFragment(pRange4, 3))
    }

    def calcFutureResult(sorter: SimpleParallelSort[Statistics, _], body1: => Unit, body2: => Unit, body3: => Unit, body4: => Unit) =
    {
        val future1 = Future { body1 }
        val future2 = Future { body2 }
        val future3 = Future { body3 }
        val future4 = Future { body4 }
        val resultFuture = for{ result1 <- future1; result2 <- future2; result3 <- future3; result4 <- future4 } yield
        {
            sorter.result
        }
        val result = Await.result(resultFuture, Duration.Inf).filter(x => x._8 > 0)
        result foreach println
        result
    }
}
