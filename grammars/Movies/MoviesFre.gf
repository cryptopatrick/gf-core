concrete MoviesFre of Movies = {
  flags coding = utf8 ;

  param
    Number = Sg | Pl ;
    Gender = Masc | Fem ;

  lincat
    S  = Str ;
    NP = {s : Str ; n : Number ; g : Gender} ;
    VP = {s : Number => Str} ;
    N  = {s : Number => Str ; g : Gender} ;
    Det = {s : Gender => Number => Str} ;

  lin
    Pred np vp = np.s ++ vp.s ! np.n ;

    UseDet det n = {s = det.s ! n.g ! Sg ++ n.s ! Sg ; n = Sg ; g = n.g} ;

    John = {s = "Jean" ; n = Sg ; g = Masc} ;
    Mary = {s = "Marie" ; n = Sg ; g = Fem} ;
    I_Pron = {s = "je" ; n = Sg ; g = Masc} ;

    Recommends np = {s = table {
      Sg => "recommande" ++ np.s ;
      Pl => "recommandent" ++ np.s
    }} ;

    Watches np = {s = table {
      Sg => "regarde" ++ np.s ;
      Pl => "regardent" ++ np.s
    }} ;

    DetA   = {s = table {
      Masc => table {Sg => "un" ; Pl => "des"} ;
      Fem  => table {Sg => "une" ; Pl => "des"}
    }} ;

    DetThe = {s = table {
      Masc => table {Sg => "le" ; Pl => "les"} ;
      Fem  => table {Sg => "la" ; Pl => "les"}
    }} ;

    Movie       = {s = table {Sg => "film" ; Pl => "films"} ; g = Masc} ;
    Film        = {s = table {Sg => "film" ; Pl => "films"} ; g = Masc} ;
    ActionMovie = {s = table {Sg => "film d'action" ; Pl => "films d'action"} ; g = Masc} ;
}

