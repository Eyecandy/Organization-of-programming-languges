package solver

object Solver extends App{
  // solves expString == 0 for the variable in varName with an initial guess
  // specified. We'll assume that the given expression has a root.
  def solve(expString: String, varName: String, guess: Double): Double = {
    val expression:Option[Expression] = Parser(expString)
    val dfex = Process.differentiate(expression.get,varName)
    val m = Map(varName -> guess)
    val f = (g:Double) => Process.eval(expression.get,Map(varName -> g))
    val df = (g:Double) => Process.eval(Process.differentiate(expression.get,varName), Map(varName -> g))
    Newton.solve(f,df,guess)
  }
}
