concrete CallForFireEng of CallForFire = {
  lincat
    Call = {obs : Str; warn : Str; loc : Str; desc : Str; engage : Str; fireCtrl : Str};
    ObserverID, WarningOrder, TargetLocation, TargetDescription, MethodEngagement, MethodFireControl = Str;

  lin
    MakeCall obs warn loc desc engage fireCtrl = {
      obs = obs;
      warn = warn;
      loc = loc;
      desc = desc;
      engage = engage;
      fireCtrl = fireCtrl
    };

    Observer id = id;
    WarnOrder mission method = mission ++ ", " ++ method;
    TargetLoc loc = loc;
    TargetDesc desc = desc;
    MethodEngage engage = engage;
    MethodFireCtrl fireCtrl = fireCtrl;

  -- Linearization rules to produce the final string
  oper
    linCall : Call -> Str;
    linCall c = c.obs ++ " | " ++ c.warn ++ " | " ++ c.loc ++ " | " ++ c.desc ++ " | " ++ c.engage ++ " | " ++ c.fireCtrl;
}
