package com.example.loto.array

/**
 * Created by alespuh on 03.01.15.
 */
object ArrayFiller
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

    def createArray(size: Int, default: Int) =
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

    private val figures = (1 to 36).toArray
}
