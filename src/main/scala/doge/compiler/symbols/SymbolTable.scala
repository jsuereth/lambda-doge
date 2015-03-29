package doge.compiler.symbols

/**
 * A tabular look up of symbols from names.
 */
trait SymbolTable {
  /** Uses the given name to lookup a symbol in the symbol table. */
  def lookup(name: String): Option[DogeSymbol]

}
