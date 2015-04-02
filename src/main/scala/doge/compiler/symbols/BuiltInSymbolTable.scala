package doge.compiler.symbols

import doge.compiler.types.TypeSystem.Type

/** A symbol table of built-in functions. */
class BuiltInSymbolTable(symbols: Seq[BuiltInFunctionSymbol]) extends SymbolTable {
  private val lookupTable: Map[String, BuiltInFunctionSymbol] = symbols.map(s => s.name -> s).toMap
  /** Uses the given name to lookup a symbol in the symbol table. */
  override def lookup(name: String): Option[DogeSymbol] = lookupTable.get(name)
}
object BuiltInSymbolTable {
  def Function(name: String, tpe: Type): BuiltInFunctionSymbol =
    new BuiltInFunctionSymbolImpl(name, tpe)
}

case class BuiltInFunctionSymbolImpl(name: String, tpe: Type) extends BuiltInFunctionSymbol {
  override def toString = s"buitin $name : $tpe}"
}

/** Attempts to check if a symbol was pruned, but the same as a builtIn. */
class BuiltInSymExtractor(orig: DogeSymbol) {
  def unapply(sym: DogeSymbol): Boolean =
    (sym == orig) ||
    (sym.isPruned && sym.original == orig)
}
