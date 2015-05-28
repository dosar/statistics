package com.example.loto.metrics

import com.example.loto.array.ArrayPerformanceUtil
import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

trait CombinedBetFigureMetrics
{
    this: MetricsTypes =>

    def topWithMiddle(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val topFigures = FiguresByHitSorter.topFigures(figuresOccurencies(rrs))
        scala.compat.Platform.arraycopy(topFigures, 0, result, 0, betSizeLimit)
        val middlePosition = (topFigures.length - result.length) / 2
        scala.compat.Platform.arraycopy(topFigures, middlePosition, result, betSizeLimit, betSizeLimit)
        result
    }

    def topWithTrueMiddle(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        scala.compat.Platform.arraycopy(topFigures, 0, result, 0, betSizeLimit)
        copyTrueMiddle(topHits, topFigures, result, betSizeLimit)
        result
    }

    def topWithZero(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        scala.compat.Platform.arraycopy(topFigures, 0, result, 0, betSizeLimit)
        copyZero(topHits, topFigures, result, betSizeLimit)
        result
    }

    def topWithLeast(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        scala.compat.Platform.arraycopy(topFigures, 0, result, 0, betSizeLimit)
        copyLeast(topHits, topFigures, result, betSizeLimit)
        result
    }

    def middleWithTrueMiddle(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        copyMiddle(topFigures, result, 0)
        copyTrueMiddle(topHits, topFigures, result, betSizeLimit)
        result
    }

    def middleWithZero(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        copyMiddle(topFigures, result, 0)
        copyZero(topHits, topFigures, result, betSizeLimit)
        result
    }

    def middleWithLeast(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        copyMiddle(topFigures, result, 0)
        copyLeast(topHits, topFigures, result, betSizeLimit)
        result
    }

    def trueMiddleWithZero(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        copyTrueMiddle(topHits, topFigures, result, 0)
        copyZero(topHits, topFigures, result, betSizeLimit)
        result
    }

    def trueMiddleWithLeast(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        copyTrueMiddle(topHits, topFigures, result, 0)
        copyLeast(topHits, topFigures, result, betSizeLimit)
        result
    }

    def zeroWithLeast(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](betSizeLimit * 2)
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        copyZero(topHits, topFigures, result, 0)
        copyLeast(topHits, topFigures, result, betSizeLimit)
        result
    }

    def getCombinedBetIntersectionStatistics(rrs: Array[RunResult], bet: CombinedBet): (StrategyStatistics, SliceSize) =
    {
        var ind = 0
        var (i2, i3, i4, i5, mplus, mminus) = (0, 0, 0, 0, 0, 0)
        val betSize = betSizeLimit
        while(ind < rrs.length)
        {
            mminus += betCost(betSize) * 2 //потому шо 2 ставки
            val rr = rrs(ind)
            val intersection1 = intersectionSize(rr.result, bet)(0, betSizeLimit)
            val intersection2 = intersectionSize(rr.result, bet)(betSizeLimit, betSizeLimit)

            if(intersection1 == 5)
                return (StrategyStatistics(i2, i3, i4, 1, 1000000 + mplus, mminus), ind + 1)
            else if(intersection1 == 2) i2 += 1
            else if(intersection1 == 3) i3 += 1
            else if(intersection1 == 4) i4 += 1

            if(intersection2 == 5)
                return (StrategyStatistics(i2, i3, i4, 1, 1000000 + mplus, mminus), ind + 1)
            else if(intersection2 == 2) i2 += 1
            else if(intersection2 == 3) i3 += 1
            else if(intersection2 == 4) i4 += 1

            mplus += betWon(betSize)(intersection1) + betWon(betSize)(intersection2)
            ind += 1
        }
        (StrategyStatistics(i2, i3, i4, i5, mplus, mminus), ind)
    }

    private def copyTrueMiddle(topHits: Array[Int], topFigures: Array[Int], result: Bet, positionInResult: Int) =
    {
        val length = ArrayPerformanceUtil.firstNonNegativeMinInd(topHits)
        val position = length - betSizeLimit
        scala.compat.Platform.arraycopy(topFigures, if(position < 0) 0 else position, result, positionInResult, betSizeLimit)
    }

    private def copyZero(topHits: Array[Int], topFigures: Array[Int], result: Bet, positionInResult: Int) =
    {
        val length = ArrayPerformanceUtil.lastNonNegativeMinInd(topHits) + 1
        val position = length - betSizeLimit
        scala.compat.Platform.arraycopy(topFigures, position, result, positionInResult, betSizeLimit)
    }

    private def copyLeast(topHits: Array[Int], topFigures: Array[Int], result: Bet, positionInResult: Int) =
    {
        val until = ArrayPerformanceUtil.lastNonNegativeMinInd(topHits) + 1
        scala.compat.Platform.arraycopy(topFigures, until - betSizeLimit, result, positionInResult, betSizeLimit)
    }

    private def copyMiddle(topFigures: Array[Int], result: Bet, positionInResult: Int) =
    {
        val middlePosition = (topFigures.length - betSizeLimit) / 2
        scala.compat.Platform.arraycopy(topFigures, middlePosition, result, positionInResult, betSizeLimit)
    }
}
