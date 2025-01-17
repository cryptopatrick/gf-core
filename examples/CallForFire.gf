abstract CallForFire = {
  cat
    Call;           -- Represents the entire call for fire
    ObserverID;     -- Observer's identification
    WarningOrder;   -- Type of mission and method of target location
    TargetLocation; -- Location of the target
    TargetDescription; -- Description of the target
    MethodEngagement;  -- Method of engagement
    MethodFireControl; -- Method of fire control

  fun
    -- Functions to construct each part of the call
    MakeCall : ObserverID -> WarningOrder -> TargetLocation -> TargetDescription -> MethodEngagement -> MethodFireControl -> Call;
    Observer : String -> ObserverID;
    WarnOrder : String -> String -> WarningOrder;
    TargetLoc : String -> TargetLocation;
    TargetDesc : String -> TargetDescription;
    MethodEngage : String -> MethodEngagement;
    MethodFireCtrl : String -> MethodFireControl;
}
