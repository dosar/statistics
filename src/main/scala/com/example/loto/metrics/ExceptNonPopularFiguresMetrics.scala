package com.example.loto.metrics

import com.example.loto.array.ArrayPerformanceUtil._
import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

trait ExceptNonPopularFiguresMetrics
{
    this: MetricsTypes =>

    def figureToIgnores: Array[Figure]

    lazy val figuresMapTemplate = createArray(37)
        { ind => if (figureToIgnores.contains(ind) || ind < startFigure || ind > endFigure) -1 else 0 }

    /* Метрика */
    def topExceptIgnored(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        FiguresByHitSorter.topFigures(fillFigureOccurencies(rrs, slice(figuresMapTemplate, 0, 37)))(betSizeLimit)
    }

    def leastPopularExceptIgnored(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(fillFigureOccurencies(rrs, slice(figuresMapTemplate, 0, 37)))
        val until = lastPositiveInd(hits) + 1
        slice(figures, until - betSizeLimit, until)
    }

    def zeroExceptIgnored(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter
            .topFiguresWithHits(fillFigureOccurencies(rrs, slice(figuresMapTemplate, 0, 37)))
        val startInd = lastPositiveInd(hits) + 1
        slice(figures, startInd, Math.min(startInd + betSizeLimit, figures.length))
    }

    //берем все-таки из середины, так шо могут и быть числа что из ignored, что вне диапазона startFigure и endFigure
    def middleExceptIgnored(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val figures = FiguresByHitSorter.topFigures(fillFigureOccurencies(rrs, slice(figuresMapTemplate, 0, 37)))
        val occInd = (figures.length - betSizeLimit) / 2
        slice(figures, occInd, occInd + betSizeLimit)
    }

    def trueMiddleExceptIgnored(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(fillFigureOccurencies(rrs, slice(figuresMapTemplate, 0, 37)))
        val occInd = (lastPositiveInd(hits) + 1 - betSizeLimit) / 2
        slice(figures, occInd, occInd + betSizeLimit)
    }
}