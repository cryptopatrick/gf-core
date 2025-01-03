{-
The abstract grammar defines the shared structure and meaning of the phrases, 
independent of any specific language.

The Grammar provide the following simple translations:
# Input in English:

### _Question:_ "Who are you?"
+   Abstract: WhoAreYou
+   English Concrete: "Who are you?"
+   Heptapod Concrete: "??-heptapod-WHO"

### Answer: "I am a human."
+   Abstract: IAm Human
+   English Concrete: "I am a human"
+   Heptapod Concrete: "!!-heptapod-I-!! HUMAN-circle-circle"

### Answer: "I come from Earth."
+   Abstract: From Earth
+   English Concrete: "I come from Earth"
+   Heptapod Concrete: "!!-heptapod-FROM-!! EARTH-spiral"

## Abstract Syntax
WhoAreYou
WhereFrom
WhatDoYouWant
IAm Human
From Earth
Want AlienPlanet
Visit Alien Earth
LiveOn Human Earth

-}
abstract Arrival = {
  cat
    Phrase;       -- General phrases
    Question;     -- Questions
    Answer;       -- Answers
    Entity;       -- Entities like humans, Earth, alien planet
    Action;       -- Actions like live, visit, etc.

  fun
    -- Questions
    WhoAreYou : Question;
    WhereFrom : Question;
    WhatDoYouWant : Question;

    -- Answers
    IAm : Entity -> Answer;
    From : Entity -> Answer;
    Want : Entity -> Answer;

    -- Entities
    Human : Entity;
    Earth : Entity;
    AlienPlanet : Entity;
    Alien : Entity;

    -- Actions (optional for added flexibility)
    LiveOn : Entity -> Action -> Phrase;
    Visit : Entity -> Entity -> Phrase;
}
