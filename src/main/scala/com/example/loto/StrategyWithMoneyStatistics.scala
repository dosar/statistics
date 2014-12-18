package com.example.loto

import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

trait StrategyWithMoneyStatistics extends MoneyHitStatisticsType
{
    def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: Vector[RunResult] => Array[Figure]):
        ArrayBuffer[(Array[Figure], (IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus))]
}
