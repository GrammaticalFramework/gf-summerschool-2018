--# -path=.:../portuguese:../abstract

resource MiniSyntaxPor = 
  MiniGrammarPor **    --- inheriting everything from Grammar, not just Cat and Structural
  MiniSyntax with
    (MiniGrammar=MiniGrammarPor) ;
