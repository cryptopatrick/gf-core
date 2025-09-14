concrete FlightFre of Flight = {

  lincat
    Utterance, Question, Answer, Booking, FlightInfo, Punct = {s : Str} ;
    City, Date = {s : Str} ;

  lin
    -- Cities
    London = {s = "Londres"} ;
    Paris = {s = "Paris"} ;
    NewYork = {s = "New York"} ;
    Tokyo = {s = "Tokyo"} ;

    -- Dates
    Today = {s = "aujourd'hui"} ;
    Tomorrow = {s = "demain"} ;
    NextWeek = {s = "la semaine prochaine"} ;

    -- Flight info
    FromTo from to = {s = "de " ++ from.s ++ " à " ++ to.s} ;
    OnDate info date = {s = info.s ++ " " ++ date.s} ;

    -- Punctuation
    QMark = {s = "?"} ;

    -- Dialogue
    AskFlight info q = {s = "Avez-vous des vols " ++ info.s ++ " " ++ q.s} ;
    ConfirmFlight info = {s = "Je voudrais réserver un vol " ++ info.s} ;
    AskPrice info = {s = "Quel est le prix pour un vol " ++ info.s ++ " ?"} ;
    GivePrice info = {s = "Le prix pour un vol " ++ info.s ++ " est 200 euros"} ;
    ConfirmBooking info = {s = "Oui, merci de confirmer la réservation " ++ info.s} ;
    SayThanks = {s = "Merci"} ;

    -- Wrapping
    UseQuestion q = {s = q.s} ;
    UseAnswer a = {s = a.s} ;
    UseBooking b = {s = b.s} ;
}

