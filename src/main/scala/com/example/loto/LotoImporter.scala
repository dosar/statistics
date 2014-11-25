package com.example.loto

import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx
import com.orientechnologies.orient.core.record.impl.ODocument

import scala.io.Source

/**
 * Created by alespuh on 25.11.14.
 */
object LotoImporter
{
    def fillDb =
    {
        val source = Source.fromFile("/home/alespuh/work/loto/src/main/resources/5_36.txt")
        val lines = source.getLines().toList
//        println("Количество линий " + lines.size)
        val lines1 = lines.zipWithIndex.filter(_._2 % 3 == 0).map(_._1).toList
        val lines2 = lines.zipWithIndex.filter(_._2 % 3 == 1).map(_._1).toList
        val db: ODatabaseDocumentTx  = new ODatabaseDocumentTx("remote:/loto").open("admin", "admin")
        try
        {
            for((line1, line2) <- lines1.zip(lines2))
            {
//                println(line1)
//                println(line2)
                val doc = new ODocument("loto_5_36")
                val run :: date :: Nil = line1.split(" / ").toList
                doc.field("run", run)
                doc.field("date", date)
                doc.field("result", line2.split(" ").filter(_.trim != "").map(_.toInt))

                doc.save()
            }
        }
        finally db.close()
    }
}