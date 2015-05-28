package com.example.loto.optimization

import com.example.loto.metrics.{MoneyHitStatisticsType, StrategyStatistics}

/**
 * Created by alespuh on 16.01.15.
 */

object Strategy5GradientDescent extends MoneyHitStatisticsType
{
    type StartFigure = Int; type EndFigure = Int
    type P1 = Double; type P2 = Double; type P3 = Double; type P4 = Double; type P5 = Double
    type PastWindow = Int; type SkipWindow = Int; type FutureWindow = Int
    type Features = (StartFigure, EndFigure, P1, P2, P3, P4, P5, PastWindow, SkipWindow, FutureWindow)

    implicit class ToPlusable(val x: Features) extends AnyVal
    {
        def +(y: Features) = (x._1 + y._1, x._2 + y._2, x._3 + y._3, x._4 + y._4, x._5 + y._5, x._6 + y._6,
            x._7 + y._7, x._8 + y._8, x._9 + y._9, x._10 + y._10)
    }
}

import Strategy5GradientDescent._

class Strategy5GradientDescent(lowBound: Features, highBound: Features)(f: Features => StrategyStatistics)
{
    def optimize(x0: Features)(iterations: Int): Features =
    {
        val gradientOptimizer = new GradientDescent(x0, iterations)
        gradientOptimizer.optimize(gradient)(_ + _)
    }

    private def gradient(x: Features): Features =
    {
        implicit val fx = f(x)
        startFigureGradient(x) + endFigureGradient(x) + p1Gradient(x) + p2Gradient(x) + p3Gradient(x) + p4Gradient(x) +
            p5Gradient(x) + pastWindowGradient(x) + skipWindowGradient(x) + futureWindowGraphic(x)
    }

    def futureWindowGraphic(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_10 = 1), zero.copy(_10 = -1))

    def skipWindowGradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_9 = 1), zero.copy(_9 = -1))

    def pastWindowGradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_8 = 1), zero.copy(_8 = -1))

    def p5Gradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_7 = 0.1), zero.copy(_7 = -0.1))

    def p4Gradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_6 = 0.1), zero.copy(_6 = -0.1))

    def p3Gradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_5 = 0.1), zero.copy(_5 = -0.1))

    def p2Gradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_4 = 0.1), zero.copy(_4 = -0.1))

    def p1Gradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_3 = 0.1), zero.copy(_3 = -0.1))

    def endFigureGradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_2 = 1), zero.copy(_2 = -1))

    def startFigureGradient(x: Features)(implicit fx: StrategyStatistics) =
        featureGradient(x, zero.copy(_1 = 1), zero.copy(_1 = -1))

    private val zero: Features = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def featureGradient(x: Features, xPlus: Features, xMinus: Features)(implicit fx: StrategyStatistics): Features =
    {
        if(inRange(x + xPlus) && f(x + xPlus) > fx) xPlus
        else if(inRange(x + xMinus) && f(x + xMinus) > fx) xMinus
        else zero
    }

    def fxStep(x: Features)(implicit fx: StrategyStatistics) =
    {
        if(inRange(x)) f(x) else fx
    }

    def inRange(x: Features) =
    {
        x._1 >= lowBound._1 && x._1 <= highBound._1 &&
        x._2 >= lowBound._2 && x._2 <= highBound._2 &&
        x._3 >= lowBound._3 && x._3 <= highBound._3 &&
        x._4 >= lowBound._4 && x._4 <= highBound._4 &&
        x._5 >= lowBound._5 && x._5 <= highBound._5 &&
        x._6 >= lowBound._6 && x._6 <= highBound._6 &&
        x._7 >= lowBound._7 && x._7 <= highBound._7 &&
        x._8 >= lowBound._8 && x._8 <= highBound._8 &&
        x._9 >= lowBound._9 && x._9 <= highBound._9 &&
        x._10 >= lowBound._10 && x._10 <= highBound._10
    }
}
