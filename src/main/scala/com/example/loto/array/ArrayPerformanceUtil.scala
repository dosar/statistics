package com.example.loto.array

/**
 * Created by alespuh on 03.01.15.
 */
object ArrayPerformanceUtil
{
    /*ожидаем, что exceptFigure отсортирован по возрастанию и в конце завершающий 0*/
    def createFiguresArray(exceptFigures: Array[Int]) =
    {
        val resultSize = 36 - exceptFigures.size + 1
        val result = new Array[Int](resultSize)
        var resultInd = 0
        var exceptFiguresInd = 0
        var figure = 1
        while(resultInd < resultSize && figure < 37)
        {
            if(figure != exceptFigures(exceptFiguresInd))
            {
                result(resultInd) = figure
                resultInd += 1
            }
            else exceptFiguresInd += 1
            figure += 1
        }
        result
    }

    def createArray(size: Int, default: => Int) =
    {
        var ind = 0
        val result = new Array[Int](size)
        while(ind < size)
        {
            result(ind) = default
            ind += 1
        }
        result
    }

    def createArray[T <: AnyRef : Manifest](size: Int, default: => T) =
    {
        var ind = 0
        val result = new Array[T](size)
        while(ind < size)
        {
            result(ind) = default
            ind += 1
        }
        result
    }

    def maxForArray(array: Array[Int]) =
    {
        var max = array(0)
        var ind = 1
        while(ind < array.length)
        {
            val value = array(ind)
            if(value > max)
                max = value
            ind += 1
        }
        max
    }

    type ExpandedArray = Array[Int]
    def safeSetArrayElement(array: Array[Int], ind: Int, value: Int)(ifCapacityExceeded: ExpandedArray => Unit): Unit =
    {
        if(ind < array.length)
            array(ind) = value
        else
        {
            val expandedArray = new Array[Int](array.length * 2)
            scala.compat.Platform.arraycopy(array, 0, expandedArray, 0, array.length)
            expandedArray(ind) = value
            ifCapacityExceeded(expandedArray)
        }
    }
}