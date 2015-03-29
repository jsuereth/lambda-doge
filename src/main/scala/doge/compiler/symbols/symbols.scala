package doge.compiler.symbols

import doge.compiler.types.TypeSystem.Type

/** A symbol in the doge world. */
sealed trait DogeSymbol {
  /** The type assocaited with this symbol. */
  def tpe: Type

  /** This returns true if the symbol represents something NOT created inside the Doge language. */
  def isJava: Boolean
  // Helpers for checking java symbols.
  def isJavaClass: Boolean
  def isJavaMethod: Boolean
  def isJavaConstructor: Boolean
  def isJavaField: Boolean
}

/** A marker trait for all symbols coming from reading JDK classes. */
sealed abstract class JavaSymbol extends DogeSymbol {
  override def isJava: Boolean = true
  override def isJavaClass: Boolean = false
  override def isJavaMethod: Boolean = false
  override def isJavaConstructor: Boolean = false
  override def isJavaField: Boolean = false
}

/** Represents a Java Class. */
abstract class JavaClassSymbol extends JavaSymbol {
  def name: String
  def tpe: Type
  def constructors: Seq[JavaConstructorSymbol]
  def methods: Seq[JavaMethodSymbol]
  def fields: Seq[JavaFieldSymbol]
  /** True if the "class" is an interface. */
  def isInterface: Boolean
  /** True if the "class" is an abstract class. */
  def isAbstract: Boolean
  /** Returns the parent class of this class, or None if we're looking at Object. or a trait. */
  def parentClass: Option[JavaClassSymbol]
  /** Returns the set of interfaces this class implements. */
  def interfaces: Seq[JavaClassSymbol]
  override def isJavaClass: Boolean = true
}
/** Represents a constructor of a java class. */
abstract class JavaConstructorSymbol extends JavaSymbol {
  /** The owner of this field. */
  def owner: JavaClassSymbol
  /* The type of the constructor (inputs -> Class) */
  def tpe: Type
  /** The arity of the constructor, in case we get confused in the type system, as we implicitly curry. */
  def arity: Int
  override def isJavaConstructor: Boolean = true
}
/** Represents a method of a Java class. */
abstract class JavaMethodSymbol extends JavaSymbol {
  /** The name of the method. */
  def name: String
  /** The type signature of the method.
    *
    * Note, this will always care an implicit "this" parameter first.
    */
  def tpe: Type
  /** The arity of the method. */
  def arity: Int
  /** The owner of this field. */
  def owner: JavaClassSymbol
  /** True if the method is static. */
  def isStatic: Boolean
  override def isJavaMethod: Boolean = false
}
/** Represents the field of a Java class. */
abstract class JavaFieldSymbol extends  JavaSymbol {
  /** The name of the field. */
  def name: String
  /** The DOGE type for this field. */
  def tpe: Type
  /** True if the field is static. */
  def isStatic: Boolean
  /** The owner of this field. */
  def owner: JavaClassSymbol
  override def isJavaField: Boolean = true
}