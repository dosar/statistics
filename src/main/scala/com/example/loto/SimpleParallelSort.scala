package com.example.loto

import scala.reflect.ClassTag

/*
* chunks - сколько сегментов для хранения и сортировки результатов
* */
class SimpleParallelSort[TElem: ClassTag](chunks: Int = 4, chunkSize: Int = 50, default: TElem)(
    valueGetter: TElem => Int, compare: (Int, Int) => Int = _.compareTo(_))
{
    val chunkArrays = (1 to chunks).map(x => Array.fill(chunkSize)(default))
    val chunkIndexes = new Array[Int](chunks)

    def update(chunk: Int, elem: TElem): Unit =
    {
        val chunkArray = chunkArrays(chunk)
        val existed = valueGetter(chunkArray(0))
        val candidate = valueGetter(elem)
        if(existed.compareTo(candidate) < 0)
        {
            chunkArray(0) = elem
            chunkIndexes(chunk) = 1
        }
        else if(existed.compareTo(candidate) == 0 && chunkIndexes(chunk) < chunkSize)
        {
            chunkArray(chunkIndexes(chunk)) = elem
            chunkIndexes(chunk) += 1
        }
    }

    def result = chunkArrays.flatten.filter(x => valueGetter(x) > 0).sortBy(x => -valueGetter(x))
}
