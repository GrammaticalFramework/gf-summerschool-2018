--# -path=.:../afrikaans:../abstract

resource MiniSyntaxAfr =
  MiniGrammarAfr ** --- inheriting everything from Grammar, not just Cat and Structural
  MiniSyntax with
    (MiniGrammar=MiniGrammarAfr) ;
