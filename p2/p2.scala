import java.io.FileInputStream;
import java.util.Scanner;
import scala.util.Random;
import scala.collection.mutable.Map;

import scala.List;

/*
 * Abstract class the all expandable parts of
 * a grammar extend (Terminal, NonTerminal, Production, Definition).
 */
abstract class GrammarElement {
  
  /**
   * Expand the grammar element as part of a random 
   * derivation.  Use grammar to look up the definitions
   * of any non-terminals encountered during expansion.
   */
  def expand(grammar : Grammar) : String;
  
  /**
   * Return a string representation of this grammar element.
   * This is useful for debugging.  (Even though we inherit a
   * default version of toString() from the Object superclass, 
   * I include it as an abstract method here to ensure that 
   * all subclasses provide their own implementation.)
   */
  def toString() : String;	
  
}


/**
 * Represents a grammar as a map from non-terminal names (Strings) to
 * Defintions.
 */
class Grammar(map : Map[String,Definition]) {
  
  // add a new non-terminal, with the given definition
  def +=(nt : String, defn : Definition) = {
    map +=(nt -> defn);
  }
  
  // look up a non-terminal, and return the definition, or null
  // if not def exists.
  def apply(nt : String) : Definition = { map(nt) }
  
  // Expand the start symbol for the grammar.
  def expand() : String = {
    return map("<start>").expand(this);
  }
  
  // return a String representation of this object.
  override def toString() : String = {
    var str = "";
    val it = map.keysIterator;
    while (it.hasNext) {
      var cur = it.next;
      str += cur + ": " + map(cur).toString() + "\n";
    }
    return str;
  }
}

class Terminal(str : String) extends GrammarElement { 
  override def expand(grammar : Grammar) : String = { str }
  override def toString() : String = { str }
}

class NonTerminal(str : String) extends GrammarElement { 
  override def expand(grammar : Grammar) : String = {
    val defn = grammar.apply(str);
    return defn.expand(grammar);
  }

  override def toString() : String = { str }
}

class Production(list : List[GrammarElement]) extends GrammarElement { 
  override def expand(grammar : Grammar) : String = {
    var result = "";
    for (i <- 0 to list.length - 1) {
      result = list(i).expand(grammar) + " " + result;
    }
    return result;
  }

  override def toString() : String = {
    var result = "";
    var i = 0;
    for (i <- 0 to list.length - 1) {
      result = list(i).toString() + " " + result;
    }
    return result;
  }
}

class Definition(list : List[Production]) extends GrammarElement { 
  override def expand(grammar : Grammar) : String = {
    val number = Random.nextInt(list.length);
    return list(number).expand(grammar);
  }

  override def toString() : String = {
    var result = "";
    var i = 0;
    for (i <- 0 to list.length - 1) {
      result = list(i).toString() + " | " + result;
    }
    return result;
  }
}




object RandomSentenceGenerator {
  
    /**
     * Read tokens up to the end of a production and return 
     * them as a Production.
     *
     * Parses "Production ::= [ Word ]*"
     * where word is any terminal/non-terminal.
     */
  protected def readProduction(in : Scanner) : Production = {
    
    var p = List[String]();
    
    while (in.hasNext() && !(in.hasNext(";") || in.hasNext("\\|"))) {
      // word is next word in production (either a Non-Terminal or Terminal).
      val word = in.next();

      p ::= word;
      
    }

    val pp = p.map( elem => if(elem.startsWith("<")) {new NonTerminal(elem) } else {new Terminal(elem)} );
    
    return new Production(pp);
  }
  
  /**
   * Read a group of productions and return them as a Definition.
   *
   * Parses "<Definition> ::= <Production> [ '|' <Production> ]* ';'" 
   */
  def readDefinition(in : Scanner) : Definition = {
    
    var d = List[Production]();

    // production is first production for definition    
    val production = readProduction(in);

    d ::= production;
        
    while (in.hasNext("\\|")) {
      expect(in, "\\|");

      // production is the next production for definition
      val production = readProduction(in);
      
      d ::= production;
      
    }
    expect(in, ";");
    
    return new Definition(d);  			// return the Definition
  }
  
  /**
   * Repeatedly read non-terminal definitions and insert them into
   * the grammar.
   *
   * Parses "<Grammar> ::= [ <Non-Terminal> '=' <Definition> ';' ]*" 
   */
  protected def readGrammar(in : Scanner) : Grammar = {

    var A:Map[String,Definition] = Map();

    // the grammar for this generator
    var grammar = new Grammar(A);
    
    while (in.hasNext()) {
      val name = in.next();
      
      expect(in, "=");
      
      val defn = readDefinition(in);
      
      // defn is next definition to add to grammar
      grammar += (name, defn);
    }
    return grammar;
  }
  
  /**
   * A helper method than matches s to the next token returned from
   * the scanner.  If it matches, throw away the token and get ready
   * to read the next one.  If it doesn't match, generate an error.
   * 
   * Since s is used as a regular expression, be sure to escape any
   * special characters like |, which should become \\|
   */
  protected def expect(in : Scanner, s : String) = {
    if (!in.hasNext(s)) {
      println(in.next);
      RandomSentenceGenerator.fail("expected " + s);
    }
    in.next();  // skip s
  }
  

  /**
   * Helper method to abort gracefully when an error occurs.
   * <p>
   * Usage: RandomSentenceGenerator.fail("Error Message");
   */
  def fail(msg : String) = {
    throw new RuntimeException(msg);
  }
  
  /**
   * Create a random sentence generator and print out
   * three random productions.
   */
  def main(args: Array[String]) = {
    val grammar = this.readGrammar(new Scanner(scala.io.Source.stdin.mkString));

    println("Grammar is: \n" + grammar);

    println(grammar.expand());
    println(grammar.expand());
    println(grammar.expand());
  }
}
