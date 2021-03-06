package doge.compiler.symbols

import doge.compiler.types.TypeSystem
import doge.compiler.types.TypeSystem.Type

/** A symbol in the doge world.
  *
  * Symbols are generally references by name, and have an assocaited type.   The type is used
  * for type checking, and specifics about the symbol are used for generating bytecode.
  *
  * THere are several 'leaf' symbols:
  *
  * -- Doge Language Related --
  *
  * - BuiltInFunctionSymbol:  A pseudo function which directly embeds bytecode
  * - DogeFunctionSymbol: A user defined let expression
  * - FunctionParameterSymbol: A parameter available in a limited scope (of a function).
  *
  * -- Java Related --
  *
  * - JavaMethodSymbol:  A reference to a real java method (non-static are seen as functions of this -> args -> result)
  * - JavaConstructorSymbol:  A reference to a real java constructor (seen as a function from args -> instance)
  * - JavaFieldSymbol: A reference to a real java field (seen as a function from this -> field value)
  */
sealed trait DogeSymbol {
  /** All symbols must have a name for their current scope. */
  def name: String
  /** The type associated with this symbol. */
  def tpe: Type

  /** Returns true if the symbol is a built-in (i.e. hardcoded/forced) entity of the language. */
  def isBuiltIn: Boolean
  /** This returns true if the symbol represents something NOT created inside the Doge language. */
  def isJava: Boolean
  // Helpers for checking java symbols.
  def isJavaClass: Boolean
  def isJavaMethod: Boolean
  def isJavaConstructor: Boolean
  def isJavaField: Boolean

  /** Converts this symbol by modifying the type.
    * This is useful, for example, after looking up a symbol with an abstract singature and replacing with
    * a reference to a more-specific type in the type tree.
    *
    * @param tpe The new type for the symbol
    * @return The new symbol, but with the original pointing to the underlying/first symbol encountered.
    */
  def withType(tpe: Type): DogeSymbol
  def original: DogeSymbol
}



abstract class AbstractDogeSymbol extends DogeSymbol {
  override def isBuiltIn: Boolean = false
  override def isJava: Boolean = false
  override def isJavaClass: Boolean = false
  override def isJavaMethod: Boolean = false
  override def isJavaConstructor: Boolean = false
  override def isJavaField: Boolean = false
  override def original: DogeSymbol = this
}

/** Symbols generated by let expressions in the Doge language. */
abstract class DogeLanguageSymbol extends AbstractDogeSymbol {
  def name: String
}

/** A symbol denoting a user defined function.
  * Keeps track of the name, and type.
  */
abstract class DogeFunctionSymbol extends DogeLanguageSymbol {
  /** The Java classname of the class which owns this function. */
  def ownerClass: String
  def tpe: Type = TypeSystem.FunctionN(returnTpe, argTpes:_*)
  /** Argument types*/
  def argTpes: Seq[Type]
  /** return types */
  def returnTpe: Type
  // TODO - owner?  We want to know who the owning class is
}

/** A symbol to reference a parameter which should be in-scope. */
abstract class FunctionParameterSymbol extends DogeLanguageSymbol {
  /** The owning symbol of this parameter. */
  //def owner: DogeFunctionSymbol
}

/** A symbol for a built-in function.
  *
  * Built-in functions are directly generated in bytecode by the compiler, and do not exist as
  * physical entities on the JVM.   This type helps isolate them, as some special handling of partial-function
  * application must be performed for built-in functions.
  */
abstract class BuiltInFunctionSymbol extends DogeLanguageSymbol {
  override def isBuiltIn: Boolean = true
}



/** A marker trait for all symbols coming from reading JDK classes. */
sealed abstract class JavaSymbol extends AbstractDogeSymbol {
  override def isJava: Boolean = true
  override def isJavaClass: Boolean = false
  override def isJavaMethod: Boolean = false
  override def isJavaConstructor: Boolean = false
  override def isJavaField: Boolean = false
}

/** Represents a Java Class.
  *
  * TODO - Java classes really shouldn't be referenced by value.
  */
abstract class JavaClassSymbol extends JavaSymbol {
  def name: String

  def jvmName: String

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
  /** The jvm description of arguments for this constructor. */
  def jvmDesc: String
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
  /** The raw jvm descriptions of the method. */
  def jvmDesc: String
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
  /** The jvm description (for bytecode generation) for this field. */
  def jvmDesc: String
  override def isJavaField: Boolean = true
}