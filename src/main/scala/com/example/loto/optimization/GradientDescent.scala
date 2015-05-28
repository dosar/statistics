package com.example.loto.optimization

/**
 * x0 - startPoint
 */
class GradientDescent[T](x0: T, iterations: Int)
{
    /*
    * gradient - возвращает дельту к T, по которой будет следующая итерация
    * */
    def optimize(gradient: T => T)(plus:(T, T) => T): T =
    {
        var result = x0
        for(i <- 1 to iterations)
            result = plus(result, gradient(result))
        result
    }
}
