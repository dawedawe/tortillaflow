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

    type Comida =
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
          Comida: Comida option }

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
        | WhatCondition ->
            ("What is your tortilla like?", [ ("soft", ChooseCondition Soft); ("crunchy", ChooseCondition Crunchy) ])
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

    let (|ConditionChoiceNeeded|SizeAndShapeChoiceNeeded|MeatInsideChoiceNeeded|NoQuestionLeft|) model =
        match (model.Condition, model.SizeAndShape, model.Fixings, model.Folding, model.Fried) with
        | (None, None, None, None, None) -> ConditionChoiceNeeded
        | (Some Crunchy, None, None, None, None) -> SizeAndShapeChoiceNeeded
        | (Some Crunchy, Some Handsized, None, None, None) -> MeatInsideChoiceNeeded
        | _ -> NoQuestionLeft

    let (|WhatsInsideChoiceNeeded|AnyRiceChoiceNeeded|FriedChoiceNeeded|FoldingChoiceNeeded|StripsOfMeatChoiceNeeded|SauceChoiceNeeded|NoQuestionLeft|)
        model
        =
        match (model.Condition, model.SizeAndShape, model.Fixings, model.Folding, model.Fried) with
        | (Some Soft, None, None, None, None) -> WhatsInsideChoiceNeeded
        | (Some Soft, None, Some fs, None, None) when Fixings.adheresTo fs [ Meat ] -> AnyRiceChoiceNeeded
        | (Some Soft, None, Some fs, None, None) when Fixings.adheresTo fs [ Meat; Rice ] -> FriedChoiceNeeded
        | (Some Soft, None, Some fs, Some Roundish, None) when Fixings.adheresTo fs [ Meat; NoRice ] ->
            FriedChoiceNeeded
        | (Some Soft, None, Some fs, None, None) when Fixings.adheresTo fs [ Meat; NoRice ] -> FoldingChoiceNeeded
        | (Some Soft, None, Some fs, Some FlatFolded, None) when Fixings.adheresTo fs [ Meat; NoRice ] ->
            StripsOfMeatChoiceNeeded
        | (Some Soft, None, Some fs, Some FlatFolded, None) when Fixings.adheresTo fs [ Meat; NoRice; MeatStrips ] ->
            SauceChoiceNeeded
        | _ -> NoQuestionLeft

    let nextQuestion tortilla =
        match tortilla with
        | ConditionChoiceNeeded -> Some WhatCondition
        | SizeAndShapeChoiceNeeded -> Some WhatShapeAndSize
        | MeatInsideChoiceNeeded -> Some IsMeatInside
        | WhatsInsideChoiceNeeded -> Some WhatsInside
        | AnyRiceChoiceNeeded -> Some AnyRice
        | FriedChoiceNeeded -> Some IsFried
        | FoldingChoiceNeeded -> Some WhatFolding
        | StripsOfMeatChoiceNeeded -> Some HasStripsOfMeat
        | SauceChoiceNeeded -> Some HasSauceOnTop
        | _ -> None

    let (|ItsNachos|ItsTaquito|ItsTaco|ItsEmptyTacoShell|ItsNone|) model =
        match (model.Condition, model.Folding, model.Fried, model.Fixings, model.SizeAndShape) with
        | (Some Crunchy, None, None, None, Some SmallTrianglesOvalsOrRectangles) -> ItsNachos
        | (Some Crunchy, None, None, None, Some RolledUp) -> ItsTaquito
        | (Some Crunchy, None, None, Some fs, Some Handsized) when Fixings.adheresTo fs [ Meat ] -> ItsTaco
        | (Some Soft, Some Roundish, Some false, Some fs, _) when Fixings.adheresTo fs [ Meat; NoRice ] -> ItsTaco
        | (Some Soft, Some FlatFolded, None, Some fs, _) when Fixings.adheresTo fs [ Meat; NoRice; NoMeatStrips ] ->
            ItsTaco
        | (Some Crunchy, None, None, Some fs, Some Handsized) when Fixings.adheresTo fs [ Empty ] -> ItsEmptyTacoShell
        | _ -> ItsNone

    let (|ItsTortillaSoup|ItsQuesadilla|ItsBurrito|ItsChimichanga|ItsEnchilada|ItsFajita|ItsNone|) model =
        match (model.Condition, model.Folding, model.Fried, model.Fixings, model.SizeAndShape) with
        | (Some Soft, None, None, Some fs, None) when Fixings.adheresTo fs [ Soup ] -> ItsTortillaSoup
        | (Some Soft, None, None, Some fs, None) when Fixings.adheresTo fs [ Cheese ] -> ItsQuesadilla
        | (Some Soft, None, Some false, Some fs, None) when Fixings.adheresTo fs [ Meat; Rice ] -> ItsBurrito
        | (Some Soft, None, Some true, Some fs, None) when Fixings.adheresTo fs [ Meat; Rice ] -> ItsChimichanga
        | (Some Soft, Some Roundish, Some true, Some fs, None) when Fixings.adheresTo fs [ Meat; NoRice ] ->
            ItsChimichanga
        | (Some Soft, Some FlatFolded, None, Some fs, None) when
            Fixings.adheresTo fs [ Meat; NoRice; MeatStrips; SauceOnTop ]
            ->
            ItsEnchilada
        | (Some Soft, Some FlatFolded, None, Some fs, None) when
            Fixings.adheresTo fs [ Meat; NoRice; MeatStrips; NoSauceOnTop ]
            ->
            ItsFajita
        | _ -> ItsNone

    let determineComida model =
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
                  Comida = None }
              Timeline = List.empty
              History = Stack<Model>() }

        (model, Cmd.none)

    let update (msg: Msg) (model: Model) =
        match msg with
        | ChooseCondition c ->
            let tortilla = { model.Tortilla with Condition = Some c }
            let timelineEntry = Questions.getTimelineEntryForMsg model.NextQuestion.Value msg

            let model' =
                { model with
                    NextQuestion = nextQuestion tortilla
                    Timeline = timelineEntry :: model.Timeline
                    Tortilla = { tortilla with Comida = determineComida tortilla } }

            model'.History.Push(model')
            (model', Cmd.none)
        | ChooseSizeAndShap s ->
            let tortilla = { model.Tortilla with SizeAndShape = Some s }
            let timelineEntry = Questions.getTimelineEntryForMsg model.NextQuestion.Value msg

            let model' =
                { model with
                    NextQuestion = nextQuestion tortilla
                    Timeline = timelineEntry :: model.Timeline
                    Tortilla = { tortilla with Comida = determineComida tortilla } }

            model'.History.Push(model')
            (model', Cmd.none)
        | ChooseFixings x ->
            let fixings' =
                match model.Tortilla.Fixings with
                | None -> Fixings.add Fixings.Create x
                | Some fs -> Fixings.add fs x

            let tortilla = { model.Tortilla with Fixings = Some fixings' }
            let timelineEntry = Questions.getTimelineEntryForMsg model.NextQuestion.Value msg

            let model' =
                { model with
                    NextQuestion = nextQuestion tortilla
                    Timeline = timelineEntry :: model.Timeline
                    Tortilla = { tortilla with Comida = determineComida tortilla } }

            model'.History.Push(model')
            (model', Cmd.none)
        | ChooseIsFried b ->
            let tortilla = { model.Tortilla with Fried = Some b }
            let timelineEntry = Questions.getTimelineEntryForMsg model.NextQuestion.Value msg

            let model' =
                { model with
                    NextQuestion = nextQuestion tortilla
                    Timeline = timelineEntry :: model.Timeline
                    Tortilla = { tortilla with Comida = determineComida tortilla } }

            model'.History.Push(model')
            (model', Cmd.none)
        | ChooseFolding f ->
            let tortilla = { model.Tortilla with Folding = Some f }
            let timelineEntry = Questions.getTimelineEntryForMsg model.NextQuestion.Value msg

            let model' =
                { model with
                    NextQuestion = nextQuestion tortilla
                    Timeline = timelineEntry :: model.Timeline
                    Tortilla = { tortilla with Comida = determineComida tortilla } }

            model'.History.Push(model')
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

    let comidaInfos comida =
        match comida with
        | Nachos ->
            ("Nachos", "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Nachos1.jpg/1920px-Nachos1.jpg")
        | Taquito -> ("Taquito", "https://upload.wikimedia.org/wikipedia/commons/8/8b/Flautas_guacamole_tortillas.jpg")
        | Taco ->
            ("Taco",
             "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/001_Tacos_de_carnitas%2C_carne_asada_y_al_pastor.jpg/1920px-001_Tacos_de_carnitas%2C_carne_asada_y_al_pastor.jpg")
        | EmptyTacoShell ->
            ("Empty Taco Shells",
             "https://media.istockphoto.com/photos/isolated-taco-shell-picture-id700209484?k=6&m=700209484&s=170667a&w=0&h=kus7mbbg-qZUT_nplZoqXhahc04kNhPq1SgAdLRifL0=")
        | TortillaSoup ->
            ("Tortilla Soup",
             "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Tortilla_Soup.jpg/1920px-Tortilla_Soup.jpg")
        | Fajita ->
            ("Fajita",
             "https://media.istockphoto.com/photos/chicken-fajitas-picture-id477724063?k=6&m=477724063&s=170667a&w=0&h=ah81Nbum22t41X3R_z_50EymCrOVl0YJzkkefDKO5W0=")
        | Enchilada ->
            ("Enchilada", "https://upload.wikimedia.org/wikipedia/commons/b/ba/Festival_de_la_Enchilada_59.jpg")
        | Quesadilla ->
            ("Quesadilla",
             "https://upload.wikimedia.org/wikipedia/commons/thumb/7/75/Quesadilla_2.jpg/1920px-Quesadilla_2.jpg")
        | Burrito -> ("Burrito", "https://upload.wikimedia.org/wikipedia/commons/1/17/Shredded_pork_burrito.jpg")
        | Chimichanga ->
            ("Chimichanga",
             "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a8/Chimichangas.jpg/1920px-Chimichangas.jpg")

    let renderResult comida =
        let (name, url) = comidaInfos comida

        Bulma.box
            [ prop.className "result"
              prop.children
                  [ Bulma.columns
                        [ Bulma.column
                              [ Html.p "You end up with:"
                                Html.p [ prop.className "comida"; prop.text name ]
                                Html.p "Buen provecho :)" ]
                          Bulma.column [ Html.img [ prop.src url ] ] ] ] ]

    let button (text: string) m dispatch =
        Bulma.button.button [ prop.text text; prop.onClick (fun _ -> m |> dispatch) ]

    let renderQuestion question dispatch =
        let (q, answers) = Questions.getQuestion question

        [ Html.div [ Html.strong q ]
          for (a, aMsg) in answers do
              button a aMsg dispatch ]
        |> Bulma.box

    let renderTimeline entries =
        Bulma.box
            [ Html.strong "Your choices"
              Html.unorderedList
                  [ for e in entries do
                        Html.listItem [ Html.p $"{e.Question}"; Html.strong $"{e.Answer}" ]
                        Html.br [] ] ]

    let renderCard state dispatch =
        Bulma.card
            [ Bulma.cardContent
                  [ Bulma.columns
                        [ Bulma.column
                              [ if Option.isSome state.Tortilla.Comida then
                                    renderResult state.Tortilla.Comida.Value
                                else if Option.isSome state.NextQuestion then
                                    renderQuestion state.NextQuestion.Value dispatch
                                else
                                    () ]
                          Bulma.column [ renderTimeline state.Timeline ] ] ]
              Bulma.cardFooter
                  [ Bulma.cardFooterItem.div
                        [ Bulma.button.button
                              [ prop.text "Go back"
                                prop.onClick (fun _ -> GoBack |> dispatch)
                                prop.disabled (state.History.Count <= 0) ] ]
                    Bulma.cardFooterItem.div
                        [ Bulma.button.button [ prop.text "Restart"; prop.onClick (fun _ -> Restart |> dispatch) ] ] ] ]

    [<ReactComponent>]
    let ViewComp () =

        let state, dispatch = React.useElmish (State.init, State.update, [||])

        [ Bulma.title "Tortilla flow"; renderCard state dispatch ] |> Html.div

open Browser.Dom
open View

Feliz.ReactDOM.render (ViewComp, document.getElementById "elmish-app")
