package com.example.loto

import com.example.loto.array.ArrayPerformanceUtil
import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

/**
 * Created by alespuh on 09.01.15.
 */
trait PairFigureMetrics extends MoneyHitStatisticsType
{
    this: MetricsTypes =>

    type EncodedPair = Int
    def topFigureCombinedWithPair(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](topFiguresCount)
        val (pairsHit, topPairs)= FiguresByHitSorter.topFiguresWithHits(pairFiguresOccurencies(rrs))
        val (_, topFigures)= FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        var resultInd = 0
        var topFigureInd = 0
        while(resultInd < topFiguresCount)
        {
            val topFigure = topFigures(topFigureInd)
            resultInd = addToResult(result, resultInd, topFigure)
            if(addToResult(result, resultInd, topFigure) != resultInd)
                resultInd = setPairCompanion(topPairs, pairsHit, result, resultInd + 1, topFigure)
            topFigureInd += 1
        }
        result
    }

    def setPairCompanion(topPairs: Array[EncodedPair], pairHits: Array[HitCount], result: Array[Figure],
        resultInd: Int, figure: Figure): Index =
    {
        var ind = 0
        while(ind < topPairs.length && pairHits(ind) > 0)
        {
            val encodedPair = topPairs(ind)
            val fig1 = extractFigure1(encodedPair)
            val fig2 = extractFigure2(encodedPair)
            if(fig1 == figure)
            {
                if(addToResult(result, resultInd, fig2) != resultInd) return resultInd + 1
            }
            else if(fig2 == figure)
            {
                if(addToResult(result, resultInd, fig1) != resultInd) return resultInd + 1
            }
            ind += 1
        }
        resultInd
    }

    def topPairFigures(rrs: Array[RunResult]): Array[Figure] =
    {
        val result = new Array[Figure](topFiguresCount)
        val (pairsHit, topPairs)= FiguresByHitSorter.topFiguresWithHits(pairFiguresOccurencies(rrs))
        var resultInd = 0
        var topPairsInd = 0
        while(resultInd < topFiguresCount)
        {
            val encodedPair = topPairs(topPairsInd)
            val fig1 = extractFigure1(encodedPair)
            val fig2 = extractFigure2(encodedPair)
            if(fig1 != fig2 && pairsHit(topPairsInd) != -1)
            {
                resultInd = addToResult(result, resultInd, fig1)
                resultInd = addToResult(result, resultInd, fig2)
            }
            topPairsInd += 1
        }
        result
    }

    def pairFiguresOccurencies(rrs: Array[RunResult], startFigure: Int = startFigure, endFigure: Int = endFigure): Array[HitCount] =
    {
        val figuresMap = ArrayPerformanceUtil.createArray(36 * 36)
        { ind =>
            val fig1 = extractFigure1(ind)
            val fig2 = extractFigure2(ind)
            if(pairFigureMatch(fig1, fig2)) 0 else -1
        }

        var ind = 0
        while(ind < rrs.length)
        {
            updateMap(figuresMap, rrs(ind).result)
            ind += 1
        }
        figuresMap
    }

    private def addToResult(result: Array[Figure], resultInd: Int, figure: Int): Int =
    {
        if(!result.contains(figure) && resultInd < topFiguresCount)
        {
            result(resultInd) = figure
            resultInd + 1
        } else resultInd
    }

    private def extractFigure1(encodedPair: Int) = (encodedPair / 36) + 1

    private def extractFigure2(encodedPair: Int) = encodedPair % 36 + 1

    private def calcInd(fig1: Int, fig2: Int) = (fig1 - 1) * 36 + fig2 - 1

    private def updateMap(figuresMap: Array[HitCount], result: Array[Figure]): Unit =
    {
        var fig1Ind = 0
        while(fig1Ind < result.length)
        {
            var fig2Ind = fig1Ind + 1
            while(fig2Ind < result.length)
            {
                val fig1 = result(fig1Ind)
                val fig2 = result(fig2Ind)
                if(pairFigureMatch(fig1, fig2))
                    figuresMap(calcInd(fig1, fig2)) += 1
                fig2Ind += 1
            }
            fig1Ind += 1
        }
    }

    private def pairFigureMatch(fig1: Int, fig2: Int) =
        fig1 >= startFigure && fig1 <= endFigure && fig2 >= startFigure && fig2 <= endFigure
}
