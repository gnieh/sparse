package sparse

import scala.annotation.compileTimeOnly

trait Symbol[+Result] {

  protected[sparse] val id: Int

  @compileTimeOnly("`Symbol.~` can only be used in the `Grammar` section of a parser")
  final def ~[T](that: Symbol[T]): Symbol[(Result, T)] = ???

  @compileTimeOnly("`Symbol.~>` can only be used in the `Grammar` section of a parser")
  final def ~>[Out](f: Result => Out): Symbol[Out] = ???

  @compileTimeOnly("`Symbol.?` can only be used in the `Grammar` section of a parser")
  final def ? : Symbol[Option[Result]] = ???

  @compileTimeOnly("`Symbol.*` can only be used in the `Grammar` section of a parser")
  final def * : Symbol[List[Result]] = ???

  @compileTimeOnly("`Symbol.+` can only be used in the `Grammar` section of a parser")
  final def + : Symbol[List[Result]] = ???

  @compileTimeOnly("`Symbol.|` can only be used in the `Grammar` section of a parser")
  final def |[Result1 >: Result](that: Symbol[Result1]): Symbol[Result1] = ???

}
