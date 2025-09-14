concrete FlightEng of Flight = {

  lincat
    Utterance, Question, Answer, Booking, FlightInfo, Punct = {s : Str} ;
    City, Date = {s : Str} ;

  lin
    -- Cities
    London = {s = "London"} ;
    Paris = {s = "Paris"} ;
    NewYork = {s = "New York"} ;
    Tokyo = {s = "Tokyo"} ;

    -- Dates
    Today = {s = "today"} ;
    Tomorrow = {s = "tomorrow"} ;
    NextWeek = {s = "next week"} ;

    -- Flight info
    FromTo from to = {s = "from " ++ from.s ++ " to " ++ to.s} ;
    OnDate info date = {s = info.s ++ " on " ++ date.s} ;

    -- Punctuation
    QMark = {s = "?"} ;

    -- Dialogue
    AskFlight info q = {s = "Do you have flights " ++ info.s ++ " " ++ q.s} ;
    ConfirmFlight info = {s = "I would like to book a flight " ++ info.s} ;
    AskPrice info = {s = "What is the price for a flight " ++ info.s ++ "?"} ;
    GivePrice info = {s = "The price for a flight " ++ info.s ++ " is 200 euros"} ;
    ConfirmBooking info = {s = "Yes, please confirm the booking " ++ info.s} ;
    SayThanks = {s = "Thank you"} ;

    -- Wrapping
    UseQuestion q = {s = q.s} ;
    UseAnswer a = {s = a.s} ;
    UseBooking b = {s = b.s} ;
}

