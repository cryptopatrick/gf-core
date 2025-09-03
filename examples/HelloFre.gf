  concrete HelloFre of Hello = {
    lincat Greeting, Recipient = {s : Str} ;

    lin
      Hello recip = {s = "bonjour" ++ recip.s} ;
      World = {s = "le monde"} ;
      Mum = {s = "maman"} ;
      Friends = {s = "les amis"} ;
  }