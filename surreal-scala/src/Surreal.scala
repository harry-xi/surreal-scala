package top.harryxi.surreal

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import scala.NamedTuple

import scala.collection.JavaConverters.*

import com.surrealdb.{Response, RecordId, ValueMut, EntryMut}
import top.harryxi.surreal.SurrealEncoder.{summonNames, summonInstances}
import top.harryxi.surreal.HelperOps.*
import com.surrealdb.signin.Signin
import scala.annotation.publicInBinary
import com.surrealdb.UpType

object Surreal {

  def apply(url: String) =
    new Surreal(
      com.surrealdb
        .Surreal()
        .connect(url)
    )

  extension (res: Response) def toSeq = Range(0, res.size()).map(res.take(_))

  inline def summonVars[T <: Tuple](args: T): List[(String, ValueMut)] =
    inline args match
      case _: EmptyTuple => Nil
      case tup: (Tuple2[String, t] *: ts) =>
        (
          tup.head.asInstanceOf[(String, t)]._1,
          summonInline[SurrealEncoder[t]]
            .encode(tup.head.asInstanceOf[(String, t)]._2)
        )
          :: summonVars[Tuple.Tail[T]](args.tail)
      case _ =>
        scala.compiletime.error(
          "the vars should be a tuple of (string,?:SurrealEncoder)"
        )

}

class Surreal(val db: com.surrealdb.Surreal) extends AutoCloseable {
  import Surreal.*

  /**
   * Sets the namespace for the Surreal instance.
   * @param namespace the namespace to use
   * @return this Surreal instance
   * @see [[https://surrealdb.com/docs/surrealql/statements/use]]
   */
  def useNs(namespace: String) = {
    db.useNs(namespace)
    this
  }

  /**
   * Sets the database for the current Surreal instance.
   * @param database the database name to use
   * @return this Surreal instance
   * @see [[https://surrealdb.com/docs/surrealql/statements/use]]
   */
  def useDb(database: String) = {
    db.useDb(database)
    this
  }

  /**
   * Attempts to sign in to the Surreal system using the provided credentials.
   * The type of signin object determines the scope of the sign-in (Root, Namespace, or Database).
   * @param signin the credentials for signing in
   * @return a Token representing the session token after a successful sign-in
   * @see [[https://surrealdb.com/docs/surrealdb/security/authentication]]
   */
  def signin(signin: Signin) = db.signin(signin)

  /**
   * Creates a record in the database with the given RecordId as the key and the provided content as the value.
   * @param record the RecordId associated with the new record
   * @param content the content of the created record
   * @return the created value
   * @see [[https://surrealdb.com/docs/surrealql/statements/create]]
   */
  def create[T](record: RecordId, content: T)(using
      encoder: SurrealEncoder[T]
  ) = {
    val encodedContent = encoder.encode(content)
    db.create(record, encodedContent)
  }

  /**
   * Creates a record in the database with the given table and the provided content as the value.
   * @param table the table name
   * @param content the content of the created record
   * @return the created value
   * @see [[https://surrealdb.com/docs/surrealql/statements/create]]
   */
  def create[T](table: String, content: T)(using encoder: SurrealEncoder[T]) = {
    val encodedContent = encoder.encode(content)
    db.create(table, encodedContent)
  }

  /**
   * Deletes the specified table.
   * @param table the name of the table to be deleted
   * @see [[https://surrealdb.com/docs/surrealql/statements/delete]]
   */
  def delete(table: String) = db.delete(table)

  /**
   * Deletes a record identified by the provided RecordId.
   * @param record the identifier of the record to be deleted
   * @see [[https://surrealdb.com/docs/surrealql/statements/delete]]
   */
  def delete(record: RecordId) = db.delete(record)

  /**
   * Inserts a record in the database with the given target and content.
   * @param target the target table
   * @param content the content to insert
   * @return the inserted value
   * @see [[https://surrealdb.com/docs/surrealql/statements/insert]]
   */
  def insert[T](target: String, content: T)(using
      encoder: SurrealEncoder[T]
  ) = {
    val encodedContent = encoder.encode(content)
    db.insert(target, encodedContent)
  }

  /**
   * Inserts multiple records in the database with the given target and contents.
   * @param target the target table
   * @param content the sequence of contents to insert
   * @return the inserted values
   * @see [[https://surrealdb.com/docs/surrealql/statements/insert]]
   */
  def insert[T](target: String, content: Seq[T])(using
      encoder: SurrealEncoder[T]
  ) = {
    val encodedContents = content.map(encoder.encode)
    db.insert(target, encodedContents*)
  }

  /**
   * Establishes a relation between two records identified by `from` and `to` within a specified table, with content.
   * @param from the record identifier from which the relation originates
   * @param table the name of the table where the relation will be established
   * @param to the record identifier to which the relation points
   * @param content the content to attach to the relationship
   * @return the created relation value
   * @see [[https://surrealdb.com/docs/surrealql/statements/relate]]
   */
  def relate[T](from: RecordId, table: String, to: RecordId, content: T)(using
      encoder: SurrealEncoder[T]
  ) = {
    val encodedContent = encoder.encode(content)
    db.relate(from, table, to, encodedContent)
  }

  /**
   * Updates the value of a record with the specified content and update type.
   * @param record the RecordId of the thing to be updated
   * @param upType the type of update to be performed
   * @param content the new content to set for the specified record
   * @return the updated value
   * @see [[https://surrealdb.com/docs/surrealql/statements/update]]
   */
  def update[T](record: RecordId,upType:UpType, content: T)(using
      encoder: SurrealEncoder[T]
  ) = {
    val encodedContent = encoder.encode(content)
    db.update(record,upType, encodedContent)
  }

  /**
   * Inserts a new record or updates an existing record with the given content.
   * @param record the record identifier
   * @param upType the update type specifying how to handle the upsert
   * @param content the content to be inserted or updated
   * @return the resulting value after the upsert operation
   * @see [[https://surrealdb.com/docs/surrealql/statements/upsert]]
   */
  def upsert[T](record: RecordId,upType:UpType, content: T)(using
      encoder: SurrealEncoder[T]
  ) = {
    val encodedContent = encoder.encode(content)
    db.upsert(record,upType, encodedContent)
  }

  /**
   * Selects a record by its RecordId and retrieves the corresponding value.
   * @param record the unique identifier of the record to be selected
   * @return the selected value
   * @see [[https://surrealdb.com/docs/surrealql/statements/select]]
   */
  def select(record: RecordId) =
    db.select(record)

  /**
   * Selects all records from the specified table.
   * @param table the table name
   * @return a Scala sequence of results
   * @see [[https://surrealdb.com/docs/surrealql/statements/select]]
   */
  def select(table: String) =
    db.select(table).asScala


  

  /**
   * Executes a SurrealQL query with the provided variables as parameters.
   *
   * This method allows you to pass query variables as a tuple, which will be encoded and bound to the query.
   * Supports empty tuple (no variables), a single (String, T) pair, or a tuple of such pairs.
   *
   * Example usage:
   * {{{
   *   surreal.queryWith("SELECT * FROM user WHERE name = $name", ("name"->"Alice"))
   *   surreal.queryWith("SELECT * FROM user WHERE name = $name AND age = $age", ("name"->"Alice","age"-> 30))
   * }}}
   *
   * @param sql  The SurrealQL query string.
   * @param vars The tuple of variables to bind to the query.
   * @return     A sequence of query results.
   * @see        [[https://surrealdb.com/docs/surrealql/statements/select]]
   */
  inline def queryWith[Vars <: Tuple](sql: String, vars: Vars) =
    inline vars match
      case _: EmptyTuple => db.query(sql).toSeq
      case tup: Tuple2[String, t] =>
        val value = summonInline[SurrealEncoder[t]].encode(tup._2)
        db.queryBind(sql, Map(tup._1 -> value).asJava).toSeq
      case _: (Tuple2[String, t] *: ts) =>
        db.queryBind(sql, summonVars[Vars](vars).toMap.asJava).toSeq
      case _ =>
        scala.compiletime.error(
          "the vars should be a tuple of (string,?:SurrealEncoder)"
        )

  /**
   * Prepares a named variable context for parameterized queries.
   * 
   * Note that the variables you enter will be evaluated and converted immediately.
   *
   * Example usage:
   * {{{
   *   surreal.using((name = "Tom")).query("SELECT * FROM user WHERE name = $name")
   *   surreal.using(name = "Alice", age = 30).query("SELECT * FROM user WHERE name = $name AND age = $age")
   * }}}
   *
   * @param vars A NamedTuple of variables to bind.
   * @return     A QueryContext with the variables bound for query execution.
   */
  inline def using[
      Names <: Tuple,
      Values <: Tuple,
      Vars <: NamedTuple.NamedTuple[Names, Values]
  ](vars: Vars) =
    val classInfo = summonNames[Names] zip summonInstances[Values]
    buildContext(vars.asInstanceOf[Product], classInfo)

  private def buildContext(
      vars: Product,
      classInfo: List[(String, SurrealEncoder[?])]
  ): QueryContext =
    val fieldIterator = vars.productIterator

    val queryArgs = fieldIterator
      .zip(classInfo)
      .map { (value, info) =>
        val (name, instance) = info
        (name, instance.asInstanceOf[SurrealEncoder[Any]].encode(value))
      }
      .toMap
    QueryContext(db, queryArgs)

  class QueryContext private[surreal] (
      private val db: com.surrealdb.Surreal,
      private val ctx: Map[String, ?]
  ) {
    /**
     * Executes a SurrealQL query using the variables bound in this context.
     *
     * This method is designed for usage after `using`, e.g. `surreal.using(vars).query("...")`.
     *
     * @param query The SurrealQL query string.
     * @return      A sequence of query results.
     */
    def query(query: String) =
      db.queryBind(query, ctx.asJava).toSeq

    /**
      * Merges the variables from another QueryContext into this one, returning a new QueryContext.
      *
      * This allows you to combine multiple variable contexts for chainable and composable query building.
      * Useful when you want to reuse or extend variable sets across different queries.
      *
      * Example usage:
      * {{{
      *   val ctx1 = surreal.using(name = "Alice")
      *   val ctx2 = surreal.using(age = 30)
      *   val merged = ctx1 + ctx2
      *   merged.query("SELECT * FROM user WHERE name = $name AND age = $age")
      * }}}
      *
      * @param vars Another QueryContext whose variables will be merged.
      * @return     A new QueryContext containing variables from both contexts.
      */
    def + (vars: QueryContext): QueryContext =
      new QueryContext(db, ctx ++ vars.ctx)
  }

  /**
    * Executes a SurrealQL query.
    *
    * @param sql The SurrealQL query string.
    * @return    A sequence of query results.
    */
  def query(sql: String) = db.query(sql).toSeq

  override def close(): Unit =
    db.close()
}
