package com.example.loto.model

import java.text.SimpleDateFormat
import java.util.Date

case class RunResult(run: Int, date: Date, result: Array[Int])

object RunResult
{
    def apply(run: String, date: String, result: String) =
        new RunResult(run.toInt, format.parse(date), result.split(" ").filter(_.trim != "").take(5).map(_.toInt))

    private val format = new SimpleDateFormat("dd.MM.yyyy hh:mm")
}