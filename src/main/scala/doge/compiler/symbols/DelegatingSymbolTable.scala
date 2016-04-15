package doge.compiler.symbols

/**
 * A symbol table which delegates down to another one if a symbol is not found by the
 * current symbol table.
 */
abstract class DelegatingSymbolTable(val previous: SymbolTable) extends SymbolTable{
  override def lookup(name: String): Option[DogeSymbol] = {
    // Workaround for eager orElse
    lookupMyself(name) match {
      case None => previous.lookup(name)
      case x => x
    }
  }
  /** Look up just in the current scope's definitions. */
  def lookupMyself(name: String): Option[DogeSymbol]
}
