concrete PizzaOrderFre of PizzaOrder = open SyntaxFre, LexiconFre, ParadigmsFre in {
  lincat
    Conversation, PizzeriaGreeting, CustomerGreeting, Order, Pizza, DeliveryDetails, PizzeriaResponse, CustomerResponse = Utt ;
    Quantity = Det ;

  lin
    Dialogue pg cg o pr cr dd pr2 dd2 =
      mkUtt (mkText [pg, cg, o, pr, cr, dd, pr2, dd2]) ;

    -- Greetings
    HelloPizzeria = mkUtt (mkText "Bonjour ! Bienvenue chez Pizza Hut ! Que puis-je faire pour vous ?") ;
    HelloCustomer = mkUtt (mkText "PizzaHut, j'ai besoin de votre aide !") ;

    -- Quantities
    One = un_Det ;
    Two = mkDet (mkNumeral "deux") ;
    Three = mkDet (mkNumeral "trois") ;

    -- Orders
    SinglePizza q p = mkUtt (mkNP q p) ;
    MultipleOrders o1 o2 = mkUtt (mkNP et_Conj (fromUtt o1) (fromUtt o2)) ;

    -- Variants
    WouldLike o = mkUtt (mkCl (mkNP I_Pron) (mkVP (mkV2 "vouloir") (fromUtt o))) ;
    CanIHave o  = mkUtt (mkQS (mkQCl (mkVP (mkV2 "pouvoir") (mkVP (mkV2 "avoir") (fromUtt o)))))) ;
    IllTake o   = mkUtt (mkCl (mkNP I_Pron) (mkVP (mkV2 "vouloir") (mkVP (mkV2 "commander") (fromUtt o)))) ;

    -- Pizzas
    Margherita = mkN "pizza Margherita" ;
    Pepperoni  = mkN "pizza Pepperoni" ;
    FourCheese = mkN "pizza quatre fromages" ;
    Capriciosa = mkN "pizza Capriciosa" ;

    -- Delivery
    DeliverTo addr = mkUtt (mkText (addr)) ;
    PickUp = mkUtt (mkText "Je viendrai la chercher moi-même.") ;
    AskDelivery = mkUtt (mkText "À quelle adresse voulez-vous que la pizza soit livrée ?") ;
    ConfirmDeliveryTime = mkUtt (mkText "La pizza sera livrée dans 30 minutes.") ;

    -- Pizzeria responses
    AskPizzaType = mkUtt (mkText "Super ! Quelle pizza avez-vous en tête ?") ;
    ConfirmOrder q p = mkUtt (mkText [(mkUtt (mkNP q p)), (mkText "Autre chose ?")]) ;

    -- Customer responses
    NoThanks = mkUtt (mkText "Non, merci. C'est tout !") ;
    ThankYouBye = mkUtt (mkText "Merci ! Au revoir !") ;
}