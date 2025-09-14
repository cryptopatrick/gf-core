concrete MoviesEng of Movies = {
  flags coding = utf8 ;

  param
    Number = Sg | Pl ;

  lincat
    S  = Str ;
    NP = {s : Str ; n : Number} ;
    VP = {s : Number => Str} ;
    N  = {s : Number => Str} ;
    Det = {s : Number => Str} ;

  lin
    Pred np vp = np.s ++ vp.s ! np.n ;

    UseDet det n = {s = det.s ! Sg ++ n.s ! Sg ; n = Sg} ;

    John = {s = "John" ; n = Sg} ;
    Mary = {s = "Mary" ; n = Sg} ;
    I_Pron = {s = "I" ; n = Sg} ;

    Recommends np = {s = table {
      Sg => "recommends" ++ np.s ;
      Pl => "recommend" ++ np.s
    }} ;

    Watches np = {s = table {
      Sg => "watches" ++ np.s ;
      Pl => "watch" ++ np.s
    }} ;

    DetA   = {s = table {Sg => "a" ; Pl => ""}} ;
    DetThe = {s = table {Sg => "the" ; Pl => "the"}} ;

    Movie       = {s = table {Sg => "movie" ; Pl => "movies"}} ;
    Film        = {s = table {Sg => "film" ; Pl => "films"}} ;
    ActionMovie = {s = table {Sg => "action movie" ; Pl => "action movies"}} ;
}

