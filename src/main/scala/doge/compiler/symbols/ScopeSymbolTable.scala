package doge.compiler.symbols

import doge.compiler.types.TypeSystem.Type


class ScopeSymbolTable(val symbols: Seq[DogeLanguageSymbol], previous: SymbolTable) extends DelegatingSymbolTable(previous) {
  private val symbolMap = symbols.map(s => s.name -> s).toMap

  /** Look up just in the current scope's definitions. */
  override def lookupMyself(name: String): Option[DogeSymbol] =
    symbolMap get name


  override def toString = s"Symbols { ${symbols.mkString("\n")} $previous"
}
object ScopeSymbolTable {

  def apply(symbols: Seq[DogeLanguageSymbol], previous: SymbolTable): SymbolTable =
    new ScopeSymbolTable(symbols, previous)

  case class Argument(name: String, tpe: Type) extends FunctionParameterSymbol {
    override def withType(t: Type): FunctionParameterSymbol = {
      val self = this
      new Argument(name, t) {
        override def original: DogeSymbol = self
      }
    }
  }
  case class Function(name: String, argTpes: Seq[Type], returnTpe: Type, ownerClass: String) extends DogeFunctionSymbol {
    override def withType(t: Type): DogeFunctionSymbol = {
      val self = this
      new Function(name, argTpes, returnTpe, ownerClass) {
        override def tpe: Type = t
        override def original: DogeSymbol = self
      }
    }
  }
}
