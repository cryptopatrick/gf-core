abstract PizzaOrder = {
  cat
    Conversation ;
    PizzeriaGreeting ;
    CustomerGreeting ;
    Order ;
    Pizza ;
    DeliveryDetails ;
    Quantity ;
    PizzeriaResponse ;
    CustomerResponse ;

  fun
    Dialogue : PizzeriaGreeting -> CustomerGreeting -> Order -> PizzeriaResponse -> CustomerResponse -> DeliveryDetails -> PizzeriaResponse -> DeliveryDetails -> Conversation ;

    -- Greetings
    HelloPizzeria : PizzeriaGreeting ;
    HelloCustomer : CustomerGreeting ;

    -- Quantities
    One : Quantity ;
    Two : Quantity ;
    Three : Quantity ;

    -- Orders
    SinglePizza : Quantity -> Pizza -> Order ;
    MultipleOrders : Order -> Order -> Order ;

    -- Variants of asking
    WouldLike : Order -> Order ;
    CanIHave : Order -> Order ;
    IllTake : Order -> Order ;

    -- Pizza types
    Margherita : Pizza ;
    Pepperoni : Pizza ;
    FourCheese : Pizza ;
    Capriciosa : Pizza ;

    -- Delivery details
    DeliverTo : Str -> DeliveryDetails ;
    PickUp : DeliveryDetails ;
    AskDelivery : DeliveryDetails ;
    ConfirmDeliveryTime : DeliveryDetails ;

    -- Pizzeria responses
    AskPizzaType : PizzeriaResponse ;
    ConfirmOrder : Quantity -> Pizza -> PizzeriaResponse ;

    -- Customer responses
    NoThanks : CustomerResponse ;
    ThankYouBye : CustomerResponse ;
}