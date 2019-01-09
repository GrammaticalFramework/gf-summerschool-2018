resource MiniParadigmsNyn = open 
{-
	Used for overloaded functions, a higher level api
-}

-- normally opens other resources but because we use some
--licats defined in MiniGrammar
	MiniGrammarNyn,
	MiniResNyn, Prelude

in{

oper
	mkN = overload {
    mkN : Str -> Gender -> Noun = smartNoun ;
    mkN : Str -> Str -> Gender -> Noun = mkNoun;
    };


}
