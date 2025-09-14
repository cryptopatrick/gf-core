concrete PizzaOrderEng of PizzaOrder = open SyntaxEng, LexiconEng, ParadigmsEng in {
  lincat
    Conversation, PizzeriaGreeting, CustomerGreeting, Order, Pizza, DeliveryDetails, PizzeriaResponse, CustomerResponse = Utt ;
    Quantity = Det ;

  lin
    Dialogue pg cg o pr cr dd pr2 dd2 =
      mkUtt (mkText [pg, cg, o, pr, cr, dd, pr2, dd2]) ;

    -- Greetings
    HelloPizzeria = mkUtt (mkText "Hi! Welcome to Pizza Hut! What can I do for you?") ;
    HelloCustomer = mkUtt (mkText "PizzaHut, I need your help!") ;

    -- Quantities
    One = a_Det ;
    Two = mkDet (mkNumeral "two") ;
    Three = mkDet (mkNumeral "three") ;

    -- Orders
    SinglePizza q p = mkUtt (mkNP q p) ;
    MultipleOrders o1 o2 = mkUtt (mkNP and_Conj (fromUtt o1) (fromUtt o2)) ;

    -- Variants
    WouldLike o = mkUtt (mkCl (mkNP I_Pron) (mkVP (mkV2 "like") (fromUtt o))) ;
    CanIHave o  = mkUtt (mkQS (mkQCl can_VV (mkNP I_Pron) (mkVP have_V2 (fromUtt o)))) ;
    IllTake o   = mkUtt (mkCl (mkNP I_Pron) (mkVP (mkV2 "want") (mkVP (mkV2 "order") (fromUtt o)))) ;

    -- Pizzas
    Margherita = mkN "Margherita pizza" ;
    Pepperoni  = mkN "Pepperoni pizza" ;
    FourCheese = mkN "Four Cheese pizza" ;
    Capriciosa = mkN "Capriciosa pizza" ;

    -- Delivery
    DeliverTo addr = mkUtt (mkText (addr)) ;
    PickUp = mkUtt (mkText "I will pick it up myself.") ;
    AskDelivery = mkUtt (mkText "What address do you want the pizza delivered to?") ;
    ConfirmDeliveryTime = mkUtt (mkText "The pizza will be delivered in 30 minutes.") ;

    -- Pizzeria responses
    AskPizzaType = mkUtt (mkText "Great! Which pizza did you have in mind?") ;
    ConfirmOrder q p = mkUtt (mkText [(mkUtt (mkNP q p)), (mkText "Anything else?")]) ;

    -- Customer responses
    NoThanks = mkUtt (mkText "No, thank you. That's all!") ;
    ThankYouBye = mkUtt (mkText "Thank you! Bye!") ;
}