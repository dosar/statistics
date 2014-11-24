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
        val source = Source.fromURL(classOf[System].getResource("5_36.txt"))
        val lines = source.getLines()
        val lines1 = lines.zipWithIndex.filter(_._2 % 3 == 0).map(_._1)
        val lines2 = lines.zipWithIndex.filter(_._2 % 3 == 1).map(_._1)

        val db: ODatabaseDocumentTx  = new ODatabaseDocumentTx("remote:localhost/petshop").open("admin", "admin")
        try
        {
            for((line1, line2) <- lines1.zip(lines2))
            {
                val doc = new ODocument("Person")
                val run :: date :: Nil = line1.split(" / ").toList
                doc.field("run", run)
                doc.field("date", date)
                doc.field("result", line2.split(" ").map(_.toInt))

                doc.save()
            }
        }
        finally db.close()
    }
}
