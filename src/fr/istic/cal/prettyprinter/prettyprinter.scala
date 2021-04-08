package fr.istic.cal.prettyprinter

import scala.util.Try

/**
* définition d'une exception pour le cas des listes vides de commandes
*/
  case object ExceptionListeVide extends Exception
  
object Prettyprinter{
  
  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite 
   * respectivement pour une expression, une commande, un programme
   */
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s : String) : Expression = { WhileParser.analyserexpression(s)}
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s : String) : Command= {WhileParser.analysercommand(s)}  
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s : String) : Program = {WhileParser.analyserprogram(s)}

  
  
  /**
   * UN PRETTY-PRINTER POUR LE LANGAGE WHILE
   *
   */

  
  /**
   * définition d'un type pour les spécifications d'indentation
   */
  type IndentSpec = List[(String, Int)]
  

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une chaîne représentant la syntaxe concrète de l'expression
   */
  def prettyPrintExpr(expression: Expression): String =  {
    expression match {
      case Nl => "nil"
      case Cst(cste) => cste
      case VarExp(nom_cste) => nom_cste
      case Cons(argu1 , argu2) =>  "(cons " + prettyPrintExpr(argu1) + " " + prettyPrintExpr(argu2) + ")"
      case Hd(argu) => "(hd " + prettyPrintExpr(argu) + ")"
      case Tl(argu) => "(tl " + prettyPrintExpr(argu) + ")"
      case Eq(argu1, argu2) => prettyPrintExpr(argu1) + " =? " + prettyPrintExpr(argu2)
    }
  }

  
  /**
   *  FONCTIONS AUXILIAIRES DE TRAITEMENT DE CHAINES POUR L'INDENTATION DES COMMANDES
   *  OU LA PRESENTATION DU PROGRAMME
   */

  /**
   * * définition d'une valeur d'indentation par défaut
   */
   val indentDefault : Int = 1
  
  /**
   * recherche d'une valeur d'indentation dans une liste de spécifications d'indentation
   *
   * @param context une chaîne de caractères décrivant un contexte d'indentation
   * @param is une liste de spécifications d'indentation, chaque spécification étant un couple (contexte,indentation)
   * @return l'indentation correspondant à contexte
   */
  def indentSearch(context: String, is: IndentSpec): Int =  {
     is match {
       case Nil => indentDefault
       case (cpl :: reste) => if(context.compareTo(cpl._1) == 0) {cpl._2} else {indentSearch(context,reste)}
     }
   }

  
  /**
   * création d'une indentation
   *
   * @param n un nombre d'espaces
   * @return une chaîne de n espaces
   */
  def makeIndent(n: Int): String =  {
    n match {
      case 0 => ""
      case nombre => " " + makeIndent(nombre-1)
    }
  }

  
  /**
   * ajout d'une chaîne devant chaque élément d'une liste de chaînes
   *
   * @param pref une chaîne
   * @param strings une liste de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref devant chaque élément de strings
   */
  def appendStringBeforeAll(pref: String, strings: List[String]): List[String] =  {
    appendStringBeforeLast(pref, appendStringBeforeAllButLast(pref,strings))
  }

  
  /**
   * ajout d'une chaîne après chaque élément d'une liste de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après chaque élément de strings
   */
  def appendStringAfterAll(suff: String, strings: List[String]): List[String] =  {
    appendStringAfterLast(suff, appendStringAfterAllButLast(suff,strings))
  }

  
  /**
   * ajout d'une chaîne après chaque élément d'une liste de chaînes sauf le dernier
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après chaque élément de strings
   *         sauf le dernier
   */
  def appendStringAfterAllButLast(suff: String, strings: List[String]): List[String] =  {
    strings match {
      case Nil => Nil
      case dernier :: Nil => dernier :: Nil
      case elt :: reste => List(elt+suff)++appendStringAfterAllButLast(suff,reste)
    }
  }

  
  /**
   * ajout d'une chaîne après le dernier élément d'une liste de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après le dernier élément de strings
   */
  def appendStringAfterLast(suff: String, strings: List[String]): List[String] =  {
    strings match {
      case Nil => Nil
      case dernier :: Nil => dernier+suff :: Nil
      case elt :: reste => List(elt)++appendStringAfterLast(suff,reste)
    }
  }
  
  /**
   * ajout d'une chaîne avant chaque élément d'une liste de chaînes sauf le dernier
   *
   * @param pref une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref avant chaque élément de strings
   *         sauf le dernier
   */
  def appendStringBeforeAllButLast(pref: String, strings: List[String]): List[String] =  {
    strings match {
      case Nil => Nil
      case dernier :: Nil => dernier :: Nil
      case elt :: reste => List(pref+elt)++appendStringBeforeAllButLast(pref,reste)
    }
  }
  
  /**
   * ajout d'une chaîne avant le dernier élément d'une liste de chaînes
   *
   * @param pref une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref avant le dernier élément de strings
   */
  def appendStringBeforeLast(pref: String, strings: List[String]): List[String] =  {
    strings match {
      case Nil => Nil
      case dernier :: Nil => pref+dernier :: Nil
      case elt :: reste => List(elt)++appendStringBeforeLast(pref,reste)
    }
  }
  
  
  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la commande
   */
  def prettyPrintCommand(command: Command, is: IndentSpec): List[String] =  {
    command match {
      case Nop => "nop" :: Nil
      case Set(Var(nom) , expr) => nom + " := " +  prettyPrintExpr(expr)  :: Nil
      case While(expr, list) => List("while " + prettyPrintExpr(expr) + " do") ++ 
                                appendStringBeforeAll(makeIndent(indentSearch("WHILE",is)),prettyPrintCommands(list, is)) ++ List("od")
      case For(expr, list) => List("for " + prettyPrintExpr(expr) + " do") ++ 
                              appendStringBeforeAll(makeIndent(indentSearch("FOR",is)),prettyPrintCommands(list, is)) ++ List("od") 
      case If(expr, list_then, list_else) => List("if " + prettyPrintExpr(expr) + " then") ++ 
              appendStringBeforeAll(makeIndent(indentSearch("IF",is)),prettyPrintCommands(list_then, is)) ++ 
              List("else") ++ appendStringBeforeAll(makeIndent(indentSearch("IF",is)),prettyPrintCommands(list_else,is)) ++ List("fi")
    }
  }

  
  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la listes de commandes
   */
  def prettyPrintCommands(commands: List[Command], is: IndentSpec): List[String] = {
    commands match {
      case Nil => Nil
      case command :: Nil => prettyPrintCommand(command,is)
      case a :: reste => appendStringAfterLast(" ;",prettyPrintCommand(a,is)) ++ prettyPrintCommands(reste,is)
    }
  }
  
  
 /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste décrivant les paramètres d'entrée d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres d'entrée du programme
   */
 def prettyPrintIn(vars : List[Variable]): String =  {
   vars match {
     case Nil => ""
     case Var(nom_variable) :: Nil => nom_variable
     case Var(nom_variable) :: reste =>  nom_variable +", " + prettyPrintIn(reste)
   }
 }
 
 
  /**
   * @param vars : une liste décrivant les paramètres de sortie d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres de sortie du programme
   */
 def prettyPrintOut(vars : List[Variable]): String =  {
   vars match {
     case Nil => ""
     case Var(nom_variable) :: Nil => nom_variable
     case Var(nom_variable) :: reste => nom_variable +", " + prettyPrintOut(reste) 
   }
 }
 
 
  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  def prettyPrintProgram(program : Program, is: IndentSpec): List[String] = {
    program match {
      case Progr(entree,code,sortie) => List("read " + prettyPrintIn(entree)) ++ List("%") ++ 
      appendStringBeforeAll(makeIndent(indentSearch("PROGR",is)),prettyPrintCommands(code,is)) ++ List("%") ++
                                   List("write " + prettyPrintOut(sortie))
    }
  }
        
  
 /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une chaîne représentant la syntaxe concrète du programme
   */
  def prettyPrint(program : Program, is: IndentSpec): String =  {
    val liste_a_afficher : List[String] = appendStringAfterAllButLast("\n" , prettyPrintProgram(program,is))
    var res:String=""
    liste_a_afficher match {
      case Nil => ""
      case chaine :: Nil => chaine
      case chaine :: reste => for(elt <- reste) {res = res+elt} 
                              chaine + res
    }
  }
 
 val program: Program =
    Progr(
      List(Var("X")),
      List(
        Set(Var("Y"), Nl),
        While(
          VarExp("X"),
          List(
            Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))))),
      List(Var("Y")));
  val is: IndentSpec = List(("PROGR", 2), ("WHILE", 5));

 def main(args: Array[String]): Unit = {
   
   // vous pouvez ici tester manuellement vos fonctions par des print
   println(prettyPrintExpr(Cons(VarExp("Y"), Nl)))
   println(prettyPrintExpr(Cons(Hd(VarExp("X")),VarExp("Y"))))
   println(prettyPrintCommand(Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))), Nil))
   println(prettyPrintCommand(
        While(
          Eq(VarExp("X"), VarExp("Y")),
          List(Set(Var("X"), Cons(VarExp("X"), VarExp("Y"))))),
        List(("IF", 2), ("WHILE", 4))))

    println(List(
        "while X do",
        "    YY := Y ;",
        "    while YY do",
        "        Z := (cons nil Z) ;",
        "        YY := (tl YY)",
        "    od ;",
        "    X := (tl X)",
        "od").toString())
   println( prettyPrintCommand(
        While(
          VarExp("X"),
          List(
            Set(Var("YY"), VarExp("Y")),
            While(
              VarExp("YY"),
              List(
                Set(Var("Z"), Cons(Nl, VarExp("Z"))),
                Set(Var("YY"), Tl(VarExp("YY"))))),
            Set(Var("X"), Tl(VarExp("X"))))),
        List(("IF", 2), ("FOR", 4), ("WHILE", 4))))
   println( List(
        "read X",
        "%",
        "  Y := nil ;",
        "  while X do",
        "       Y := (cons (hd X) Y) ;",
        "       X := (tl X)",
        "  od",
        "%",
        "write Y"))
    println( prettyPrintProgram(program, is))
 }
}