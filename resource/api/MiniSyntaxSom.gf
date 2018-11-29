--# -path=.:../somali:../abstract

resource MiniSyntaxSom =
  MiniGrammarSom ** --- inheriting everything from Grammar, not just Cat and Structural
  MiniSyntax with
    (MiniGrammar=MiniGrammarSom) ;
