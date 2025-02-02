package top.harryxi.surreal

import com.surrealdb.RecordId
import org.apache.commons.text.StringEscapeUtils

object RecordIdHelper:
  def toSqlStr(recordId: RecordId) =
    import top.harryxi.surreal.SurreaEncoder.given
    s"<record>\"${(recordId.toString() |> summon[SurrealEncoder[String]].encode)}\""
