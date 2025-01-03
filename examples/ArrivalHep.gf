{-
The Heptapod concrete grammar creates an alien-style representation. 
Let’s assume Heptapod uses symbolic and repetitive structures.
-}

concrete ArrivalHeptapod of Arrival = {
  lincat
    Phrase, Answer = {s : Str};
    Question = {q : Str};
    Entity, Action = {e : Str};

  lin
    -- Questions
    WhoAreYou = {q = "??-heptapod-WHO"};
    WhereFrom = {q = "??-heptapod-FROM"};
    WhatDoYouWant = {q = "??-heptapod-WANT"};

    -- Answers
    IAm e = {s = "!!-heptapod-I-!! " ++ e.e};
    From e = {s = "!!-heptapod-FROM-!! " ++ e.e};
    Want e = {s = "!!-heptapod-WANT-!! " ++ e.e};

    -- Entities
    Human = {e = "HUMAN-circle-circle"};
    Earth = {e = "EARTH-spiral"};
    AlienPlanet = {e = "PLANET-hepta"};
    Alien = {e = "ALIEN-star"};

    -- Actions (optional)
    LiveOn e1 a = {s = e1.e ++ " LIVE-ON-!! " ++ a.e};
    Visit e1 e2 = {s = e1.e ++ " VISIT-!! " ++ e2.e};
}
