package com.example.loto.metrics

import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter
import com.example.loto.array.ArrayPerformanceUtil._

/**
 * Created by alespuh on 15.02.15.
 */
trait DistinctBetFigureMetrics extends ComplexBetFigureMetrics with ProbabalisticIntervalsMetrics
{
    this: MetricsTypes =>

    def all5WithFixedBet(rrs: Array[RunResult]): Array[Figure] =
    {
        val (hits, figures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        val result = slice(figures, 0, 25)
        copyTrueMiddle(figures, hits, result, 5, 5)
        copyMiddle(figures, result, 10, 5)
        copyLeast(figures, hits, result, 15, 5)
        copyZero(figures, hits, result, 20, 5)
        result
    }

    override val endSize: HitCount = 5

    override val probabilities: Array[Double] = Array()

    override val betSizeLimit: HitCount = 5
}
