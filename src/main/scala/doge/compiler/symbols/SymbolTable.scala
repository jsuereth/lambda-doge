package doge.compiler.symbols

/**
 * A tabular look up of symbols from names.
 *
 * Nesting these creates "scope" in the compiler.
 */
trait SymbolTable {
  /** Uses the given name to lookup a symbol in the symbol table. */
  def lookup(name: String): Option[DogeSymbol]
}

object SymbolTable {
  def join(first: SymbolTable, second: SymbolTable): SymbolTable =
    new SymbolTable {
      override def lookup(name: String): Option[DogeSymbol] =
        first.lookup(name) match {
          case None => second.lookup(name)
          case x => x
        }
    }
}
