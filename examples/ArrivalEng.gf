concrete ArrivalEng of Arrival = {
  lincat
    Phrase, Answer = {s : Str};
    Question = {q : Str};
    Entity, Action = {e : Str};

  lin
    -- Questions
    WhoAreYou = {q = "Who are you?"};
    WhereFrom = {q = "Where are you from?"};
    WhatDoYouWant = {q = "What do you want?"};

    -- Answers
    IAm e = {s = "I am " ++ e.e};
    From e = {s = "I come from " ++ e.e};
    Want e = {s = "I want " ++ e.e};

    -- Entities
    Human = {e = "a human"};
    Earth = {e = "Earth"};
    AlienPlanet = {e = "the alien planet"};
    Alien = {e = "an alien"};

    -- Actions (optional)
    LiveOn e1 a = {s = e1.e ++ " lives on " ++ a.e};
    Visit e1 e2 = {s = e1.e ++ " visits " ++ e2.e};
}
