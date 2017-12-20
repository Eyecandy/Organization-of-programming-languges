package solver


object Process {
  // gives a "pretty-print" string form of the expression
  def stringify(e: Expression): String = e match {
    case Constant(c) => c.toString
    case Var(name) => name
    case Sum(l, r) => stringify(l) + " + " + stringify(r)
    case Prod(l @ Sum(_, _), r @ Sum(_, _)) => "(" + stringify(l) + ") * (" + stringify(r) + ")"
    case Prod(l @ Sum(_, _), r) => "(" + stringify(l) + ") * " + stringify(r)
    case Prod(l, r @ Sum(_, _)) => stringify(l) + " * (" + stringify(r) + ")"
    case Prod(l, r) => stringify(l) + " * " + stringify(r)
    case Power(b, e) => stringify(b) + "^" + stringify(e)
  }

  // evaluates a given expression e: Expression using
  // the variable settings in varAssn: Map[String, Double],
  // returning the evaluation result as a Double.

  // Example: eval(e, Map("x" -> 4.0)) evaluates the expression 
  // with the variable "x" set to 4.0.
  def eval(e:Expression,varAss:Map[String,Double]): Double = e match {
    case Constant(v) => v
    case Var(v) => varAss(v)
    case Sum(e1,e2) => eval(e1,varAss) + eval(e2,varAss)
    case Prod(e1,e2) => eval(e1,varAss) * eval(e2,varAss)
    case Power(e1,e2) => Math.pow(eval(e1,varAss), eval(e2,varAss))
  }

  // symbolically differentiates an expression e: Expression with 
  // respect to the variable varName: String
  def differentiate(e: Expression, varName: String): Expression =
    simplify(e) match{
    case Constant(v) => Constant(0)
    case Var(`varName`) => Constant(1)
    case Sum(e1,e2) =>  Sum(differentiate(e1,varName),differentiate(e2,varName))
    case Prod(e1,e2) => Sum( Prod(differentiate(e1,varName),e2) , Prod(e1, differentiate(e2,varName) ) )
    case Power(fx,Constant(h)) => Prod( Prod( Constant(h), Power( fx,Constant(h-1) )),  differentiate(fx,varName))
    case Power(Constant(v),fx) =>  Prod(Constant(math.log(v)), Power(Constant(v),fx))
    case _ => {
      Constant(1)
    }
  }

  // forms a new expression that simplifies the given expression e: Expression
  // the goal of this function is produce an expression that is easier to
  // evaluate and/or differentiate.  If there's a canonical form you'd like to
  // follow, use this function to put an expression in this form.
  // you may leave this function blank if can't find any use. 
  def simplify(e: Expression): Expression = e match {
    case Sum(Constant(0),e2) =>  e2
    case Sum(e1,Constant(0)) =>  e1
    case Prod(Constant(0),_) => Constant(0)
    case Prod(_,Constant(0)) => Constant(0)
    case Power(Constant(0),_) => Constant(1)
    case Power(Constant(1),e2) => e2
    case e1 => e1
  }
}
