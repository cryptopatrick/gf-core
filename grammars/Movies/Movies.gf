abstract Movies = {
  flags startcat = S ;

  cat
    S ;     -- Sentence
    NP ;    -- Noun Phrase
    VP ;    -- Verb Phrase
    N ;     -- Noun
    Det ;   -- Determiner

  fun
    -- Sentences
    Pred : NP -> VP -> S ;

    -- Noun Phrases
    UseDet : Det -> N -> NP ;
    John, Mary, I_Pron : NP ;

    -- Verbs
    Recommends : NP -> VP ;
    Watches    : NP -> VP ;

    -- Determiners
    DetA, DetThe : Det ;

    -- Nouns
    Movie, Film, ActionMovie : N ;
}

