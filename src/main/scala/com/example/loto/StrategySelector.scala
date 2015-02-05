package com.example.loto

import com.example.loto.metrics._
import com.example.loto.model.RunResults

import scala.collection.mutable.ArrayBuffer

/**
 * Created by alespuh on 06.01.15.
 */
class StrategySelector(sf: Int, ef: Int, tcf: Int, excludeFigures: String, pw: Int, sw: Int, fw: Int, strType: String, mType: String, eSize: Int)
    extends MoneyHitStatisticsType with MetricsTypes
{
    def result: Array[(MaxIntersection, MaxIntersectionCount)] =
    {
        if(strType == "Strategy1")
        {
            val strategy = new Strategy1(RunResults.runResults, tcf, sf, ef)
            if(mType == "TopNonZeroFiguresExceptSome")
            {
                val st = new Strategy1(RunResults.runResults, tcf, sf, ef) with ExceptNonPopularFiguresMetrics
                {
                    override val figureToIgnores: Array[Figure] = figuresToIgnore
                }
                st.apply(pw, sw, fw)(st.topExceptIgnored)
            }
            else if(mType == "MiddleOccurencyFigures") strategy(pw, sw, fw)(strategy.middleOccurencyFigures)
            else if(mType == "ZeroOccurencyFigures") strategy(pw, sw, fw)(strategy.zeroOccurencyFigures)
            else if(mType == "TopNonZeroFiguresGeneric1") strategy(pw, sw, fw)(strategy.topNonZeroFiguresGenericIS)
            else if(mType == "TopNonZeroFiguresGeneric") strategy(pw, sw, fw)(strategy.topNonZeroFiguresGeneric)
            else throw new Error("unexpected pair strategy -> metric")
        }.map(_._2)
        else if(strType == "Strategy2")
        {
            throw new Error("unexpected pair strategy -> metric")
        }
        else if(strType == "Strategy3")
        {
            val strategy = new Strategy3(RunResults.runResults, tcf, sf, ef)
            if(mType == "TopNonZeroFiguresExceptSome")
            {
                val st = new Strategy3(RunResults.runResults, tcf, sf, ef) with ExceptNonPopularFiguresMetrics
                {
                    override val figureToIgnores: Array[Figure] = figuresToIgnore
                }
                st(pw, sw, fw)(st.topExceptIgnored)
            }
            else if(mType == "MiddleOccurencyFigures") strategy(pw, sw, fw)(strategy.middleOccurencyFigures)
            else if(mType == "ZeroOccurencyFigures") strategy(pw, sw, fw)(strategy.zeroOccurencyFigures)
            else if(mType == "TopNonZeroFiguresGeneric1") strategy(pw, sw, fw)(strategy.topNonZeroFiguresGenericIS)
            else if(mType == "TopNonZeroFiguresGeneric") strategy(pw, sw, fw)(strategy.topNonZeroFiguresGeneric)
            else throw new Error("unexpected pair strategy -> metric")
        }.map(_._2)
        else if(strType == "Strategy4") extractMaxIntersections
        {
            val strategy = new Strategy4(RunResults.runResults, tcf, sf, ef)
            val strategyWithBetComplex = new Strategy4(RunResults.runResults, tcf, sf, ef) with ComplexBetFigureMetrics
            {
                def endSize = eSize
            }
            val strategyWithExceptIgnored = new Strategy4(RunResults.runResults, tcf, sf, ef) with ExceptNonPopularFiguresMetrics
            {
                def figureToIgnores = figuresToIgnore
            }
            if(mType == "TopNonZeroFiguresExceptSome")
            {
                val st = new Strategy4(RunResults.runResults, tcf, 1, 36) with ExceptNonPopularFiguresMetrics
                {
                    override val figureToIgnores: Array[Figure] = figuresToIgnore
                }
                st.apply(pw, sw, fw)(st.topExceptIgnored)
            }
            else if(mType == "MiddleOccurencyFigures") strategy(pw, sw, fw)(strategy.middleOccurencyFigures)
            else if(mType == "ZeroOccurencyFigures") strategy(pw, sw, fw)(strategy.zeroOccurencyFigures)
            else if(mType == "TopNonZeroFiguresGeneric1") strategy(pw, sw, fw)(strategy.topNonZeroFiguresGenericIS)
            else if(mType == "TopNonZeroFiguresGeneric") strategy(pw, sw, fw)(strategy.topNonZeroFiguresGeneric)
            else if(mType == "TopAndMiddle") strategy(pw, sw, fw)(strategyWithBetComplex.topAndMiddle)
            else if(mType == "TopAndTrueMiddle") strategy(pw, sw, fw)(strategyWithBetComplex.topAndTrueMiddle)
            else if(mType == "TopAndLeastPopular") strategy(pw, sw, fw)(strategyWithBetComplex.topAndLeastPopular)
            else if(mType == "TopAndZero") strategy(pw, sw, fw)(strategyWithBetComplex.topAndZero)
            else if(mType == "MiddleAndTrueMiddle") strategy(pw, sw, fw)(strategyWithBetComplex.middleAndTrueMiddle)
            else if(mType == "MiddleAndLeast") strategy(pw, sw, fw)(strategyWithBetComplex.middleAndLeast)
            else if(mType == "MiddleAndZero") strategy(pw, sw, fw)(strategyWithBetComplex.middleAndZero)
            else if(mType == "TrueMiddleAndLeast") strategy(pw, sw, fw)(strategyWithBetComplex.trueMiddleAndLeast)
            else if(mType == "TrueMiddleAndZero") strategy(pw, sw, fw)(strategyWithBetComplex.trueMiddleAndZero)
            else if(mType == "LeastAndZero") strategy(pw, sw, fw)(strategyWithBetComplex.leastAndZero)
            else if(mType == "TopAndMiddlePartiallyFixedBet") strategy(pw, sw, fw)(strategyWithBetComplex.topAndMiddlePartiallyFixedBet)
            else if(mType == "TopExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.topExceptIgnored)
            else if(mType == "LeastPopularExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.leastPopularExceptIgnored)
            else if(mType == "ZeroExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.zeroExceptIgnored)
            else if(mType == "MiddleExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.middleExceptIgnored)
            else if(mType == "TrueMiddleExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.trueMiddleExceptIgnored)
            else throw new Error("unexpected pair strategy -> metric")
        }
        else if(strType == "Strategy5") extractMaxIntersections
        {
            val strategy = new Strategy5(RunResults.runResults, tcf, sf, ef)
            val strategyWithBetComplex = new Strategy5(RunResults.runResults, tcf, sf, ef) with ComplexBetFigureMetrics
            {
                def endSize = eSize
            }
            val strategyWithExceptIgnored = new Strategy5(RunResults.runResults, tcf, sf, ef) with ExceptNonPopularFiguresMetrics
            {
                def figureToIgnores = figuresToIgnore
            }
            if(mType == "TopNonZeroFiguresExceptSome")
            {
                val st = new Strategy5(RunResults.runResults, tcf, sf, ef) with ExceptNonPopularFiguresMetrics
                {
                    override val figureToIgnores: Array[Figure] = figuresToIgnore
                }
                st.apply(pw, sw, fw)(st.topExceptIgnored)
            }
            else if(mType == "MiddleOccurencyFigures") strategy(pw, sw, fw)(strategy.middleOccurencyFigures)
            else if(mType == "ZeroOccurencyFigures") strategy(pw, sw, fw)(strategy.zeroOccurencyFigures)
            else if(mType == "TopNonZeroFiguresGeneric1") strategy(pw, sw, fw)(strategy.topNonZeroFiguresGenericIS)
            else if(mType == "TopNonZeroFiguresGeneric") strategy(pw, sw, fw)(strategy.topNonZeroFiguresGeneric)
            else if(mType == "TopAndMiddle") strategy(pw, sw, fw)(strategyWithBetComplex.topAndMiddle)
            else if(mType == "TopAndTrueMiddle") strategy(pw, sw, fw)(strategyWithBetComplex.topAndTrueMiddle)
            else if(mType == "TopAndLeastPopular") strategy(pw, sw, fw)(strategyWithBetComplex.topAndLeastPopular)
            else if(mType == "TopAndZero") strategy(pw, sw, fw)(strategyWithBetComplex.topAndZero)
            else if(mType == "MiddleAndTrueMiddle") strategy(pw, sw, fw)(strategyWithBetComplex.middleAndTrueMiddle)
            else if(mType == "MiddleAndLeast") strategy(pw, sw, fw)(strategyWithBetComplex.middleAndLeast)
            else if(mType == "MiddleAndZero") strategy(pw, sw, fw)(strategyWithBetComplex.middleAndZero)
            else if(mType == "TrueMiddleAndLeast") strategy(pw, sw, fw)(strategyWithBetComplex.trueMiddleAndLeast)
            else if(mType == "TrueMiddleAndZero") strategy(pw, sw, fw)(strategyWithBetComplex.trueMiddleAndZero)
            else if(mType == "LeastAndZero") strategy(pw, sw, fw)(strategyWithBetComplex.leastAndZero)
            else if(mType == "TopAndMiddlePartiallyFixedBet") strategy(pw, sw, fw)(strategyWithBetComplex.topAndMiddlePartiallyFixedBet)
            else if(mType == "TopExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.topExceptIgnored)
            else if(mType == "LeastPopularExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.leastPopularExceptIgnored)
            else if(mType == "ZeroExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.zeroExceptIgnored)
            else if(mType == "MiddleExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.middleExceptIgnored)
            else if(mType == "TrueMiddleExceptIgnored") strategy(pw, sw, fw)(strategyWithExceptIgnored.trueMiddleExceptIgnored)
            else throw new Error("unexpected pair strategy -> metric")
        }
        else throw new Error("unexpected pair strategy -> metric")
    }.toArray

    def extractMaxIntersections(input: ArrayBuffer[(Array[Figure], StrategyStatistics)]) = input.map
    { case (figures, statistics) =>
        val StrategyStatistics(hit2, hit3, hit4, hit5, _, _) = statistics
        if(hit5 > 0) (5, hit5)
        else if(hit4 > 0) (4, hit4)
        else if(hit3 > 0) (3, hit3)
        else if(hit2 > 0) (2, hit2)
        else (0, 0)
    }

    private val figuresToIgnore = excludeFigures.split(",").map(_.trim.toInt)

    override def betSizeLimit: Int = 42
}
