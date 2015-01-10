package com.example.loto

import com.example.loto.array.ArrayPerformanceUtil
import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

/**
 * Created by alespuh on 10.01.15.
 */
trait ProbabalisticIntervalsMetrics
{
    this: MetricsTypes =>

    /*заполнен вероятностями для доверительных интервалов. индекс 0 - вероятность для интервала первого шара, 1 - второго и т.д. */
    def probabilities: Array[Double]

    def probabalisticIntervalFigures(rrs: Array[RunResult]): Bet =
    {
        if(betSizeLimit != 6) throw new Error("умеем работать только с 6 для betSizeLimit")
        val result = new Array[Int](30)
        val topFigures = FiguresByHitSorter.topFigures(figuresOccurencies(rrs))
        val intervals = Array(figuresTrustedInterval(rrs, 0, probabilities(0)),
            figuresTrustedInterval(rrs, 1, probabilities(1)),
            figuresTrustedInterval(rrs, 2, probabilities(2)),
            figuresTrustedInterval(rrs, 3, probabilities(3)),
            figuresTrustedInterval(rrs, 4, probabilities(4)))
        val firstBet = calcBet(topFigures, intervals, -1)
        val length = intervals.length
        scala.compat.Platform.arraycopy(firstBet, 0, result, 0, length)
        scala.compat.Platform.arraycopy(calcBet(topFigures, intervals, firstBet(0)), 0, result, length, length)
        scala.compat.Platform.arraycopy(calcBet(topFigures, intervals, firstBet(1)), 0, result, 2 * length, length)
        scala.compat.Platform.arraycopy(calcBet(topFigures, intervals, firstBet(2)), 0, result, 3 * length, length)
        scala.compat.Platform.arraycopy(calcBet(topFigures, intervals, firstBet(3)), 0, result, 4 * length, length)
        scala.compat.Platform.arraycopy(calcBet(topFigures, intervals, firstBet(4)), 0, result, 5 * length, length)
        result
    }

    private def appendStatistics(rr: RunResult, bet: CombinedBet, betNumber: Int, statisticsArray: Array[Int]): Boolean =
    {
        val intersection = intersectionSize(rr.result, bet)((betNumber - 1) * 5, 5)

        if(intersection == 5)
        {
            statisticsArray(3) += 1
            statisticsArray(4) += 1000000
            return true
        }
        else if(intersection == 2)
        {
            statisticsArray(0) += 1
            statisticsArray(4) += betWon(5)(2)
        }
        else if(intersection == 3)
        {
            statisticsArray(1) += 1
            statisticsArray(4) += betWon(5)(3)
        }
        else if(intersection == 4)
        {
            statisticsArray(2) += 1
            statisticsArray(4) += betWon(5)(4)
        }
        false
    }

    def getCombinedBetIntersectionStatistics(rrs: Array[RunResult], bet: CombinedBet): (StrategyStatistics, SliceSize) =
    {
        def toTuple(stat: Array[Figure]) = (stat(0), stat(1), stat(2), stat(3), stat(4), stat(5))

        var ind = 0
        //        var (i2, i3, i4, i5, mplus, mminus) = (0, 0, 0, 0, 0, 0)
        val statisticsArray = Array(0, 0, 0, 0, 0, 0)
        val betSize = betSizeLimit
        while(ind < rrs.length)
        {
            statisticsArray(5) += betCost(5) * 6 //потому шо 6 ставок по 5
            val rr = rrs(ind)
            if(appendStatistics(rr, bet, 1, statisticsArray))
                return (toTuple(statisticsArray), ind + 1)
            if(appendStatistics(rr, bet, 2, statisticsArray))
                return (toTuple(statisticsArray), ind + 1)
            if(appendStatistics(rr, bet, 3, statisticsArray))
                return (toTuple(statisticsArray), ind + 1)
            if(appendStatistics(rr, bet, 4, statisticsArray))
                return (toTuple(statisticsArray), ind + 1)
            if(appendStatistics(rr, bet, 5, statisticsArray))
                return (toTuple(statisticsArray), ind + 1)
            if(appendStatistics(rr, bet, 6, statisticsArray))
                return (toTuple(statisticsArray), ind + 1)

            ind += 1
        }
        (toTuple(statisticsArray), ind)
    }

    def figuresTrustedInterval(rrs: Array[RunResult], figureInd: Int, probability: Double): (Int, Int) =
    {
        val figureOccurencies = partialFiguresOccurencies(rrs, figureInd)
        //        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figureOccurencies)
        val hitSum = ArrayPerformanceUtil.sumNonNegativeValues(figureOccurencies)
        var hitMinus = 0.0
        var leftIndex = startFigure
        var rightIndex = ArrayPerformanceUtil.lastPositiveInd(figureOccurencies)
        var rightAdditionalHits = 0
        var leftAdditionalHits = 0
        while((hitSum - hitMinus) / hitSum > probability)
        {
            val leftHits = figureOccurencies(leftIndex)
            val rightHits = figureOccurencies(rightIndex)
            if((leftHits + leftAdditionalHits) <= (rightHits + rightAdditionalHits))
            {
                rightAdditionalHits = 0
                hitMinus += leftHits
                leftAdditionalHits += leftHits
                leftIndex += 1
            }
            else
            {
                leftAdditionalHits = 0
                hitMinus += rightHits
                rightAdditionalHits += rightHits
                rightIndex -= 1
            }
        }
        leftIndex = forwardLeftIndex(leftIndex, figureOccurencies)
        rightIndex = backwardRightIndex(rightIndex, figureOccurencies)
        if(leftIndex > rightIndex)
            (rrs(0).result(figureInd), rrs(0).result(figureInd))
        else (leftIndex, rightIndex)
    }

    private def calcBet(topFigures: Array[Figure], intervals: Array[(Int, Int)], exceptFigure: Int) =
    {
        val result = new Array[Int](intervals.length)
        var intervalInd = 0

        def matchInterval(figure: Figure, interval: (Int, Int)) = figure >= interval._1 && figure <= interval._2

        while(intervalInd < intervals.length)
        {
            var figureInd = 0
            val interval = intervals(intervalInd)
            while(!matchInterval(topFigures(figureInd), interval) &&
                (exceptFigure == -1 || topFigures(figureInd) != exceptFigure) &&
                (intervalInd == 0 || topFigures(figureInd) > result(intervalInd)))
            {
                figureInd += 1
            }
            result(intervalInd)= topFigures(figureInd)
            intervalInd += 1
        }
        result
    }

    private def forwardLeftIndex(leftIndex: Int, arr: Array[Figure]) =
    {
        var ind = leftIndex
        while(ind < arr.length && arr(ind) == 0)
            ind += 1
        ind
    }

    private def backwardRightIndex(rightIndex: Int, arr: Array[Figure]) =
    {
        var ind = rightIndex
        while(ind > -1 && arr(ind) == 0)
            ind -= 1
        ind
    }
}
