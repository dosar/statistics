package com.example.loto.metrics

import com.example.loto.array.ArrayPerformanceUtil._
import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

/**
 * Created by alespuh on 20.01.15.
 */
trait ComplexBetFigureMetrics
{
    this: MetricsTypes =>

    def endSize: Int //сколько чисел брать из другой метрики

    def topAndMiddle(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val figures = FiguresByHitSorter.topFigures(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyMiddle(figures, result, betSizeLimit - endSize, endSize)
        result
    }

    def topAndTrueMiddle(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyTrueMiddle(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def topAndLeastPopular(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyLeast(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def topAndZero(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyZero(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def middleAndTrueMiddle(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyMiddle(figures, result, 0, betSizeLimit - endSize)
        copyTrueMiddle(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def middleAndLeast(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyMiddle(figures, result, 0, betSizeLimit - endSize)
        copyLeast(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def middleAndZero(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyMiddle(figures, result, 0, betSizeLimit - endSize)
        copyZero(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def trueMiddleAndLeast(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyTrueMiddle(figures, hits, result, 0, betSizeLimit - endSize)
        copyLeast(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def trueMiddleAndZero(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyTrueMiddle(figures, hits, result, 0, betSizeLimit - endSize)
        copyZero(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def leastAndZero(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, betSizeLimit)
        copyLeast(figures, hits, result, 0, betSizeLimit - endSize)
        copyZero(figures, hits, result, betSizeLimit - endSize, endSize)
        result
    }

    def topAndMiddlePartiallyFixedBet(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(template, 0, betSizeLimit)
        var resultInd = 2

        var figureInd = (topFigures.length - endSize) / 2
        while(figureInd < topFigures.length && resultInd < 2 + endSize)
        {
            if(!contains(result, topFigures(figureInd), resultInd))
            {
                result(resultInd) = topFigures(figureInd)
                resultInd += 1
            }
            figureInd += 1
        }

        figureInd = 0
        while(resultInd < betSizeLimit && figureInd < topFigures.length)
        {
            if(!contains(result, topFigures(figureInd), resultInd))
            {
                result(resultInd) = topFigures(figureInd)
                resultInd += 1
            }
            figureInd += 1
        }
        result
    }

    private def copyMiddle(figures: Array[Int], result: Array[Figure], positionInResult: Int, length: Int) =
    {
        val middleStartInd = (figures.length - endSize) / 2
        scala.compat.Platform.arraycopy(figures, middleStartInd, result, positionInResult, length)
    }

    private def copyTrueMiddle(figures: Array[Int], hits: Array[Int], result: Array[Figure], positionInResult: Int, length: Int) =
    {
        val nonZeroHitsLength = lastNonNegativeMinInd(hits)  + 1
        val middleStartInd = (nonZeroHitsLength - endSize) / 2
        scala.compat.Platform.arraycopy(figures, middleStartInd, result, positionInResult, length)
    }

    private def copyLeast(figures: Array[Int], hits: Array[Int], result: Array[Figure], positionInResult: Int, length: Int) =
    {
        val startInd = lastPositiveInd(hits) - endSize + 1
        scala.compat.Platform.arraycopy(figures, startInd, result, positionInResult, length)
    }

    private def copyZero(figures: Array[Int], hits: Array[Int], result: Array[Figure], positionInResult: Int, length: Int) =
    {
        val startInd = lastNonNegativeMinInd(hits) - endSize + 1
        scala.compat.Platform.arraycopy(figures, startInd, result, positionInResult, length)
    }

    private def contains(arr: Array[Int], figure: Int, until: Int): Boolean =
    {
        var ind = 0
        while(ind < until)
        {
            if(arr(ind) == figure) return true
            ind += 1
        }
        return false
    }

    private val template = createArray(betSizeLimit)(
    { x =>
        if(x == 0) fixed1
        else if (x == 1) fixed2
        else 0
    })

    private lazy val fixed1 = 1
    private lazy val fixed2 = 36
}
