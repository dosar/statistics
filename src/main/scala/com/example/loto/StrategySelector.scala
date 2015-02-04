package com.example.loto

import com.example.loto.metrics._
import com.example.loto.model.RunResults

import scala.collection.mutable.ArrayBuffer

/**
 * Created by alespuh on 06.01.15.
 */
class StrategySelector(sf: Int, ef: Int, tcf: Int, excludeFigures: String, pw: Int, sw: Int, fw: Int, strType: String, mType: String)
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
            else throw new Error("unexpected pair strategy -> metric")
        }
        else if(strType == "Strategy5") extractMaxIntersections
        {
            val strategy = new Strategy5(RunResults.runResults, tcf, sf, ef)
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
