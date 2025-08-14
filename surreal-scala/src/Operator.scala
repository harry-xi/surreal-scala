package top.harryxi.surreal

private[surreal] object HelperOps {
  /** Pipe operator from ocaml */
  extension [T](x: T) inline def |>[U](f: T => U) = f(x)
}