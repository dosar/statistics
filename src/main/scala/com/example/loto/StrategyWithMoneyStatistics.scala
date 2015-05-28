package com.example.loto

import com.example.loto.metrics.{MoneyHitStatisticsType, StrategyStatistics}
import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

trait StrategyWithMoneyStatistics[TRunResults, TFigures] extends MoneyHitStatisticsType
{
    def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: TRunResults => TFigures)(implicit
        intersectionStatistics: (Array[RunResult], TFigures) => (StrategyStatistics, SliceSize)): ArrayBuffer[(Array[Figure], StrategyStatistics)]
}
