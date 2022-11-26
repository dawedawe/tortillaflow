module App

module Model =

    open System.Collections.Generic

    type Folding =
        | Roundish
        | FlatFolded

    type Fried = bool

    type Feature =
        | Empty
        | Meat
        | MeatStrips
        | NoMeatStrips
        | Cheese
        | Rice
        | NoRice
        | Soup
        | SauceOnTop
        | NoSauceOnTop

    type Fixings =
        private
        | Features of Feature Set

        static member Create = Features Set.empty

    module Fixings =
        let add (fixings: Fixings) (toAdd: Feature) =
            match (toAdd, fixings) with
            | (Empty, Features fs) when not (Set.isEmpty fs) -> System.InvalidOperationException() |> raise
            | (MeatStrips, Features fs) when Set.contains NoMeatStrips fs -> System.InvalidOperationException() |> raise
            | (NoMeatStrips, Features fs) when Set.contains MeatStrips fs -> System.InvalidOperationException() |> raise
            | (Rice, Features fs) when Set.contains NoRice fs -> System.InvalidOperationException() |> raise
            | (NoRice, Features fs) when Set.contains Rice fs -> System.InvalidOperationException() |> raise
            | (SauceOnTop, Features fs) when Set.contains NoSauceOnTop fs -> System.InvalidOperationException() |> raise
            | (NoSauceOnTop, Features fs) when Set.contains SauceOnTop fs -> System.InvalidOperationException() |> raise
            | (f, Features fs) -> Features(Set.add f fs)

        let adheresTo (fixings: Fixings) (wanted: Feature list) =
            match fixings with
            | Features fs when fs = Set.ofList wanted -> true
            | _ -> false

    type SizeAndShape =
        | SmallTrianglesOvalsOrRectangles
        | RolledUp
        | Handsized

    type Condition =
        | Crunchy
        | Soft

    type Dish =
        | Nachos
        | Taquito
        | Taco
        | EmptyTacoShell
        | TortillaSoup
        | Fajita
        | Enchilada
        | Quesadilla
        | Burrito
        | Chimichanga

    type Question =
        | WhatCondition
        | WhatShapeAndSize
        | IsMeatInside
        | WhatsInside
        | AnyRice
        | IsFried
        | WhatFolding
        | HasStripsOfMeat
        | HasSauceOnTop

    type Tortilla =
        { Condition: Condition option
          Folding: Folding option
          Fried: Fried option
          Fixings: Fixings option
          SizeAndShape: SizeAndShape option
          Dish: Dish option }

    type TimelineEntry = { Question: string; Answer: string }

    type Model =
        { NextQuestion: Question option
          Tortilla: Tortilla
          Timeline: TimelineEntry list
          History: Stack<Model> }

    type Msg =
        | ChooseCondition of Condition
        | ChooseSizeAndShap of SizeAndShape
        | ChooseFixings of Feature
        | ChooseIsFried of bool
        | ChooseFolding of Folding
        | Restart
        | GoBack

module Questions =

    open Model

    let getQuestion q =
        match q with
        | WhatCondition -> ("What is your tortilla like?", [ ("soft", ChooseCondition Soft); ("crunchy", ChooseCondition Crunchy) ])
        | WhatShapeAndSize ->
            ("What shape and size?",
             [ ("small triangles, ovals or rectangles", ChooseSizeAndShap SmallTrianglesOvalsOrRectangles)
               ("But I can't tell, it's all rolled up!", ChooseSizeAndShap RolledUp)
               ("The size of someone's hand I guess.", ChooseSizeAndShap Handsized) ])
        | IsMeatInside ->
            ("Is there meat inside?",
             [ ("Darn tootin'! (Yes)", ChooseFixings Meat)
               ("No. It's empty.", ChooseFixings Empty) ])
        | WhatsInside ->
            ("What's inside?",
             [ ("mostly meat", ChooseFixings Meat)
               ("mostly cheese", ChooseFixings Cheese)
               ("This is a SOUP!", ChooseFixings Soup) ])
        | AnyRice -> ("Any rice?", [ ("yup", ChooseFixings Rice); ("negative", ChooseFixings NoRice) ])
        | IsFried -> ("Is it fried?", [ ("yes", ChooseIsFried true); ("gross. no.", ChooseIsFried false) ])
        | WhatFolding ->
            ("How is it folded?",
             [ ("round-ish", ChooseFolding Roundish)
               ("flat, folded in half", ChooseFolding FlatFolded) ])
        | HasStripsOfMeat ->
            ("Strips of meat?",
             [ ("no", ChooseFixings NoMeatStrips)
               ("yeah, actually", ChooseFixings MeatStrips) ])
        | HasSauceOnTop -> ("Sauce on top?", [ ("no", ChooseFixings NoSauceOnTop); ("yes", ChooseFixings SauceOnTop) ])

    let getTimelineEntryForMsg question msg =
        let q, answers = getQuestion question
        let a = answers |> List.find (fun (_, aMsg) -> aMsg = msg) |> fst
        { Question = q; Answer = a }

module State =

    open System.Collections.Generic
    open Elmish

    open Model

    let (|WhatConditionIsNext|WhatSizeAndShapeIsNext|IsMeatInsideIsNext|NoQuestionLeft|) model =
        match (model.Condition, model.SizeAndShape, model.Fixings, model.Folding, model.Fried) with
        | (None, None, None, None, None) -> WhatConditionIsNext
        | (Some Crunchy, None, None, None, None) -> WhatSizeAndShapeIsNext
        | (Some Crunchy, Some Handsized, None, None, None) -> IsMeatInsideIsNext
        | _ -> NoQuestionLeft

    let (|WhatsInsideIsNext|AnyRiceIsNext|IsFriedIsNext|WhatFoldingIsNext|HasStripsOfMeatIsNext|HasSauceOnTopIsNext|NoQuestionLeft|) model =
        match (model.Condition, model.SizeAndShape, model.Fixings, model.Folding, model.Fried) with
        | (Some Soft, None, None, None, None) -> WhatsInsideIsNext
        | (Some Soft, None, Some fs, None, None) when Fixings.adheresTo fs [ Meat ] -> AnyRiceIsNext
        | (Some Soft, None, Some fs, None, None) when Fixings.adheresTo fs [ Meat; Rice ] -> IsFriedIsNext
        | (Some Soft, None, Some fs, Some Roundish, None) when Fixings.adheresTo fs [ Meat; NoRice ] -> IsFriedIsNext
        | (Some Soft, None, Some fs, None, None) when Fixings.adheresTo fs [ Meat; NoRice ] -> WhatFoldingIsNext
        | (Some Soft, None, Some fs, Some FlatFolded, None) when Fixings.adheresTo fs [ Meat; NoRice ] -> HasStripsOfMeatIsNext
        | (Some Soft, None, Some fs, Some FlatFolded, None) when Fixings.adheresTo fs [ Meat; NoRice; MeatStrips ] -> HasSauceOnTopIsNext
        | _ -> NoQuestionLeft

    let nextQuestion tortilla =
        match tortilla with
        | WhatConditionIsNext -> Some WhatCondition
        | WhatSizeAndShapeIsNext -> Some WhatShapeAndSize
        | IsMeatInsideIsNext -> Some IsMeatInside
        | WhatsInsideIsNext -> Some WhatsInside
        | AnyRiceIsNext -> Some AnyRice
        | IsFriedIsNext -> Some IsFried
        | WhatFoldingIsNext -> Some WhatFolding
        | HasStripsOfMeatIsNext -> Some HasStripsOfMeat
        | HasSauceOnTopIsNext -> Some HasSauceOnTop
        | _ -> None

    let (|ItsNachos|ItsTaquito|ItsTaco|ItsEmptyTacoShell|ItsNone|) model =
        match (model.Condition, model.Folding, model.Fried, model.Fixings, model.SizeAndShape) with
        | (Some Crunchy, None, None, None, Some SmallTrianglesOvalsOrRectangles) -> ItsNachos
        | (Some Crunchy, None, None, None, Some RolledUp) -> ItsTaquito
        | (Some Crunchy, None, None, Some fs, Some Handsized) when Fixings.adheresTo fs [ Meat ] -> ItsTaco
        | (Some Soft, Some Roundish, Some false, Some fs, _) when Fixings.adheresTo fs [ Meat; NoRice ] -> ItsTaco
        | (Some Soft, Some FlatFolded, None, Some fs, _) when Fixings.adheresTo fs [ Meat; NoRice; NoMeatStrips ] -> ItsTaco
        | (Some Crunchy, None, None, Some fs, Some Handsized) when Fixings.adheresTo fs [ Empty ] -> ItsEmptyTacoShell
        | _ -> ItsNone

    let (|ItsTortillaSoup|ItsQuesadilla|ItsBurrito|ItsChimichanga|ItsEnchilada|ItsFajita|ItsNone|) model =
        match (model.Condition, model.Folding, model.Fried, model.Fixings, model.SizeAndShape) with
        | (Some Soft, None, None, Some fs, None) when Fixings.adheresTo fs [ Soup ] -> ItsTortillaSoup
        | (Some Soft, None, None, Some fs, None) when Fixings.adheresTo fs [ Cheese ] -> ItsQuesadilla
        | (Some Soft, None, Some false, Some fs, None) when Fixings.adheresTo fs [ Meat; Rice ] -> ItsBurrito
        | (Some Soft, None, Some true, Some fs, None) when Fixings.adheresTo fs [ Meat; Rice ] -> ItsChimichanga
        | (Some Soft, Some Roundish, Some true, Some fs, None) when Fixings.adheresTo fs [ Meat; NoRice ] -> ItsChimichanga
        | (Some Soft, Some FlatFolded, None, Some fs, None) when Fixings.adheresTo fs [ Meat; NoRice; MeatStrips; SauceOnTop ] -> ItsEnchilada
        | (Some Soft, Some FlatFolded, None, Some fs, None) when Fixings.adheresTo fs [ Meat; NoRice; MeatStrips; NoSauceOnTop ] -> ItsFajita
        | _ -> ItsNone

    let determineDish model =
        match model with
        | ItsNachos -> Some Nachos
        | ItsTaquito -> Some Taquito
        | ItsTaco -> Some Taco
        | ItsEmptyTacoShell -> Some EmptyTacoShell
        | ItsTortillaSoup -> Some TortillaSoup
        | ItsQuesadilla -> Some Quesadilla
        | ItsBurrito -> Some Burrito
        | ItsChimichanga -> Some Chimichanga
        | ItsEnchilada -> Some Enchilada
        | ItsFajita -> Some Fajita
        | _ -> None

    let init () =
        let model =
            { NextQuestion = Some WhatCondition
              Tortilla =
                { Condition = None
                  Folding = None
                  Fried = None
                  Fixings = None
                  SizeAndShape = None
                  Dish = None }
              Timeline = List.empty
              History = Stack<Model>() }

        (model, Cmd.none)

    let updateModel msg model f =
        let tortilla = f model.Tortilla
        let timelineEntry = Questions.getTimelineEntryForMsg model.NextQuestion.Value msg

        let model' =
            { model with
                NextQuestion = nextQuestion tortilla
                Timeline = timelineEntry :: model.Timeline
                Tortilla = { tortilla with Dish = determineDish tortilla } }

        model'.History.Push(model')
        model'

    let update (msg: Msg) (model: Model) =
        match msg with
        | ChooseCondition c ->
            let f = fun t -> { t with Condition = Some c }
            let model' = updateModel msg model f
            (model', Cmd.none)
        | ChooseSizeAndShap s ->
            let f = fun t -> { t with SizeAndShape = Some s }
            let model' = updateModel msg model f
            (model', Cmd.none)
        | ChooseFixings x ->
            let fixings' =
                match model.Tortilla.Fixings with
                | None -> Fixings.add Fixings.Create x
                | Some fs -> Fixings.add fs x

            let f = fun t -> { t with Fixings = Some fixings' }
            let model' = updateModel msg model f
            (model', Cmd.none)
        | ChooseIsFried b ->
            let f = fun t -> { t with Fried = Some b }
            let model' = updateModel msg model f
            (model', Cmd.none)
        | ChooseFolding x ->
            let f = fun t -> { t with Folding = Some x }
            let model' = updateModel msg model f
            (model', Cmd.none)
        | Restart -> init ()
        | GoBack ->
            if model.History.Count >= 2 then
                model.History.Pop() |> ignore
                let currentModel = model.History.Peek()
                (currentModel, Cmd.none)
            else
                init ()

module View =

    open Feliz
    open Feliz.Bulma
    open Feliz.UseElmish

    open Model

    let dishInfos dish =
        match dish with
        | Nachos -> ("Nachos", "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Nachos1.jpg/1920px-Nachos1.jpg")
        | Taquito -> ("Taquito", "https://upload.wikimedia.org/wikipedia/commons/8/8b/Flautas_guacamole_tortillas.jpg")
        | Taco ->
            ("Taco",
             "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/001_Tacos_de_carnitas%2C_carne_asada_y_al_pastor.jpg/1920px-001_Tacos_de_carnitas%2C_carne_asada_y_al_pastor.jpg")
        | EmptyTacoShell ->
            ("Empty Taco Shells",
             "https://media.istockphoto.com/photos/isolated-taco-shell-picture-id700209484?k=6&m=700209484&s=170667a&w=0&h=kus7mbbg-qZUT_nplZoqXhahc04kNhPq1SgAdLRifL0=")
        | TortillaSoup -> ("Tortilla Soup", "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Tortilla_Soup.jpg/1920px-Tortilla_Soup.jpg")
        | Fajita ->
            ("Fajita",
             "https://media.istockphoto.com/photos/chicken-fajitas-picture-id477724063?k=6&m=477724063&s=170667a&w=0&h=ah81Nbum22t41X3R_z_50EymCrOVl0YJzkkefDKO5W0=")
        | Enchilada -> ("Enchilada", "https://upload.wikimedia.org/wikipedia/commons/b/ba/Festival_de_la_Enchilada_59.jpg")
        | Quesadilla -> ("Quesadilla", "https://upload.wikimedia.org/wikipedia/commons/thumb/7/75/Quesadilla_2.jpg/1920px-Quesadilla_2.jpg")
        | Burrito -> ("Burrito", "https://upload.wikimedia.org/wikipedia/commons/1/17/Shredded_pork_burrito.jpg")
        | Chimichanga -> ("Chimichanga", "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a8/Chimichangas.jpg/1920px-Chimichangas.jpg")

    let renderResult dish =
        let (name, url) = dishInfos dish

        Bulma.box
            [ prop.className "result"
              prop.children
                  [ Bulma.columns
                        [ Bulma.column
                              [ Html.p "You end up with:"
                                Html.p [ prop.className "dish"; prop.text name ]
                                Html.p "Buen provecho :)" ]
                          Bulma.column [ Html.img [ prop.src url ] ] ] ] ]

    let button (text: string) m dispatch =
        Bulma.button.button [ prop.text text; prop.onClick (fun _ -> m |> dispatch); color.isInfo ]

    let renderQuestion question dispatch =
        let (q, answers) = Questions.getQuestion question

        Html.div
            [ Html.div [ Html.strong q ]
              for (a, aMsg) in answers do
                  button a aMsg dispatch ]

    let renderTimeline entries =
        Bulma.box
            [ Html.strong "Your choices"
              Html.unorderedList
                  [ for e in entries do
                        Html.listItem [ Html.p $"{e.Question}"; Html.strong $"{e.Answer}" ]
                        Html.br [] ] ]

    let renderLeft state dispatch =
        Bulma.card
            [ Bulma.cardContent
                  [ if Option.isSome state.Tortilla.Dish then
                        renderResult state.Tortilla.Dish.Value
                    else if Option.isSome state.NextQuestion then
                        renderQuestion state.NextQuestion.Value dispatch
                    else
                        () ]
              Bulma.cardFooter
                  [ Bulma.cardFooterItem.div
                        [ Bulma.button.button
                              [ prop.onClick (fun _ -> GoBack |> dispatch)
                                color.isWarning
                                prop.disabled (state.History.Count <= 0)
                                prop.children
                                    [ Bulma.icon [ Html.i [ prop.className "fas fa-step-backward" ] ]
                                      Html.span "Previous question" ] ]
                          Bulma.button.button
                              [ prop.onClick (fun _ -> Restart |> dispatch)
                                color.isDanger
                                prop.children [ Bulma.icon [ Html.i [ prop.className "fas fa-sync" ] ]; Html.span "Restart" ] ] ] ] ]

    let renderCard state dispatch =
        Bulma.card
            [ Bulma.cardContent
                  [ Bulma.columns
                        [ Bulma.column [ renderLeft state dispatch ]
                          Bulma.column [ renderTimeline state.Timeline ] ] ] ]

    [<ReactComponent>]
    let ViewComp () =

        let state, dispatch = React.useElmish (State.init, State.update, [||])

        [ Bulma.title "Tortilla flow"; renderCard state dispatch ] |> Html.div

open Browser.Dom
open View

Feliz.ReactDOM.render (ViewComp, document.getElementById "elmish-app")
