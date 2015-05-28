package com.example.loto.model

/**
 * Created by alespuh on 08.12.14.
 */
object RunResults
{
    lazy val runResults = RunResultService.list.map(rr => rr.copy(result = rr.result.sorted)).sortBy(_.run).toArray
}
