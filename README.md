Diffe
=====

differential expression - base example of using Lisp and Java together

I was used Jatha library http://jatha.sourceforge.net/ that provide Common List power on Java platform.
Basis example is:

public String evalResult(String expression, String dif)
  {	
		LispValue resultat = null;
		try
		{
			String in = "(mydif '" + expression + " '" + dif + ")";
			LispValue T = myLisp.load(this.loadLispSource("path to lisp code file")); 
			resultat = myLisp.eval(in);
		}
		catch(Throwable ex)
		{
			// ...
		}
		return resultat.toString();
	}