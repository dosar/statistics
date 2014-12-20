package com.example.loto

import scala.collection.mutable.ArrayBuffer

trait StrategyWithMoneyStatistics[TRunResults, TFigures] extends MoneyHitStatisticsType
{
    def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: TRunResults => TFigures):
        ArrayBuffer[(Array[Figure], (IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus))]
}
