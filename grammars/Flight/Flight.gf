abstract Flight = {

  flags startcat = Utterance ;

  cat
    Utterance ;      -- top level dialogue sentences
    Question ;       -- questions like "Do you have flights?"
    Answer ;         -- responses like "Yes, we have flights"
    Booking ;        -- booking statements
    City ;           -- cities of travel
    Date ;           -- dates of departure
    FlightInfo ;     -- composed information about flight
    Punct ;          -- punctuation (e.g. ?)

  fun
    -- Cities
    London, Paris, NewYork, Tokyo : City ;

    -- Dates (simplified)
    Today, Tomorrow, NextWeek : Date ;

    -- Flight info
    FromTo : City -> City -> FlightInfo ;
    OnDate : FlightInfo -> Date -> FlightInfo ;

    -- Punctuation
    QMark : Punct ;

    -- Dialogue functions
    AskFlight : FlightInfo -> Punct -> Question ;
    ConfirmFlight : FlightInfo -> Booking ;
    AskPrice : FlightInfo -> Question ;
    GivePrice : FlightInfo -> Answer ;
    ConfirmBooking : FlightInfo -> Booking ;
    SayThanks : Utterance ;

    -- Wrapping
    UseQuestion : Question -> Utterance ;
    UseAnswer   : Answer -> Utterance ;
    UseBooking  : Booking -> Utterance ;
}

