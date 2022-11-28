module App

module Model =

    open System.Collections.Generic

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

    type Folding =
        | Roundish
        | FlatFolded

    type Fried = bool

    type SizeAndShape =
        | SmallTrianglesOvalsOrRectangles
        | RolledUp
        | Handsized
        | Round

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
        | Tostada

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

    type Language =
        | English
        | Spanish

    type Model =
        { NextQuestion: Question option
          Tortilla: Tortilla
          Timeline: TimelineEntry list
          History: Stack<Model>
          Language: Language }

    type Msg =
        | ChooseCondition of Condition
        | ChooseSizeAndShap of SizeAndShape
        | ChooseFixings of Feature
        | ChooseIsFried of bool
        | ChooseFolding of Folding
        | Restart
        | GoBack
        | ChooseLanguage of Language

module I18n =

    open Model

    [<RequireQualifiedAccess>]
    type AppString =
        | PrevQuestion
        | Restart
        | Result
        | Choices

    let translate (language: Language) (s: AppString) =
        match (language, s) with
        | (English, AppString.PrevQuestion) -> "Previous question"
        | (English, AppString.Restart) -> "Restart"
        | (English, AppString.Result) -> "You end up with"
        | (English, AppString.Choices) -> "Your choices"
        | (Spanish, AppString.PrevQuestion) -> "Pregunta anterior"
        | (Spanish, AppString.Restart) -> "Reiniciar"
        | (Spanish, AppString.Result) -> "Tu tortilla es"
        | (Spanish, AppString.Choices) -> "Tus elecciones"

    let translateQuestion lang q =
        match lang with
        | English ->
            match q with
            | WhatCondition -> "What is your tortilla like?"
            | WhatShapeAndSize -> "What shape and size?"
            | IsMeatInside -> "Is there meat inside?"
            | WhatsInside -> "What's inside?"
            | AnyRice -> "Any rice?"
            | IsFried -> "Is it fried?"
            | WhatFolding -> "How is it folded?"
            | HasStripsOfMeat -> "Strips of meat?"
            | HasSauceOnTop -> "Sauce on top?"
        | Spanish ->
            match q with
            | WhatCondition -> "¿Cómo es su tortilla?"
            | WhatShapeAndSize -> "¿Qué forma y tamaño?"
            | IsMeatInside -> "¿Hay carne adentro?"
            | WhatsInside -> "¿Qué hay adentro?"
            | AnyRice -> "¿Hay arroz?"
            | IsFried -> "¿Está frito?"
            | WhatFolding -> "¿Cómo se dobla?"
            | HasStripsOfMeat -> "¿Tiene tiras de carne?"
            | HasSauceOnTop -> "¿Tiene salsa encima?"

    let translateAnswer lang q a =
        match a with
        | ChooseCondition c ->
            match lang with
            | English ->
                match c with
                | Crunchy -> "crunchy"
                | Soft -> "soft"
            | Spanish ->
                match c with
                | Crunchy -> "crujiente"
                | Soft -> "suave"
        | ChooseSizeAndShap s ->
            match lang with
            | English ->
                match s with
                | SmallTrianglesOvalsOrRectangles -> "small triangles, ovals or rectangles"
                | RolledUp -> "But I can't tell, it's all rolled up!"
                | Handsized -> "The size of someone's hand I guess."
                | Round -> "It's just very, very round"
            | Spanish ->
                match s with
                | SmallTrianglesOvalsOrRectangles -> "Triángulos, ovals o rectángulos pequeños"
                | RolledUp -> "Pero no puedo decirlo, está todo enrollado!"
                | Handsized -> "El tamaño de la mano de alguien, supongo."
                | Round -> "Es muy, muy redondo"
        | ChooseFixings f ->
            match lang with
            | English ->
                match f with
                | Empty -> "No. It's empty."
                | Meat when q = IsMeatInside -> "Darn tootin'! (Yes)"
                | Meat -> "Mostly meat"
                | MeatStrips -> "yeah, actually"
                | NoMeatStrips -> "no"
                | Cheese -> "mostly cheese"
                | Rice -> "yup"
                | NoRice -> "negative"
                | Soup -> "This is a SOUP!"
                | SauceOnTop -> "yes"
                | NoSauceOnTop -> "no"
            | Spanish ->
                match f with
                | Empty -> "No. Está vacío."
                | Meat when q = IsMeatInside -> "Oh SI!"
                | Meat -> "Mayormente carne"
                | MeatStrips -> "Sí, en realidad"
                | NoMeatStrips -> "no"
                | Cheese -> "Mayormente queso"
                | Rice -> "sí"
                | NoRice -> "negativo"
                | Soup -> "¡Esto es una SOPA!"
                | SauceOnTop -> "sí"
                | NoSauceOnTop -> "no"
        | ChooseIsFried f ->
            match lang with
            | English ->
                match f with
                | true -> "yes"
                | false -> "gross. no."
            | Spanish ->
                match f with
                | true -> "sí"
                | false -> "no"
        | ChooseFolding f ->
            match lang with
            | English ->
                match f with
                | Roundish -> "round-ish"
                | FlatFolded -> "flat, folded in half"
            | Spanish ->
                match f with
                | Roundish -> "redondo"
                | FlatFolded -> "plana, doblada por la mitad"
        | _ -> failwith "Not implemented"

module Questions =

    open Model

    let getQuestion lang q =
        let translatedQ = I18n.translateQuestion lang q

        match q with
        | WhatCondition ->
            let answers =
                [ ChooseCondition Soft; ChooseCondition Crunchy ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)
        | WhatShapeAndSize ->
            let answers =
                [ ChooseSizeAndShap SmallTrianglesOvalsOrRectangles
                  ChooseSizeAndShap RolledUp
                  ChooseSizeAndShap Handsized
                  ChooseSizeAndShap Round ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)
        | IsMeatInside ->
            let answers =
                [ ChooseFixings Meat; ChooseFixings Empty ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)
        | WhatsInside ->
            let answers =
                [ ChooseFixings Meat; ChooseFixings Cheese; ChooseFixings Soup ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)
        | AnyRice ->
            let answers =
                [ ChooseFixings Rice; ChooseFixings NoRice ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)
        | IsFried ->
            let answers =
                [ ChooseIsFried true; ChooseIsFried false ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)
        | WhatFolding ->
            let answers =
                [ ChooseFolding Roundish; ChooseFolding FlatFolded ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)
        | HasStripsOfMeat ->
            let answers =
                [ ChooseFixings NoMeatStrips; ChooseFixings MeatStrips ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)
        | HasSauceOnTop ->
            let answers =
                [ ChooseFixings NoSauceOnTop; ChooseFixings SauceOnTop ]
                |> List.map (fun a -> (I18n.translateAnswer lang q a, a))

            (translatedQ, answers)

    let getTimelineEntry lang question msg =
        let q, answers = getQuestion lang question
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

    let (|ItsNachos|ItsTostada|ItsTaquito|ItsTaco|ItsEmptyTacoShell|ItsNone|) model =
        match (model.Condition, model.Folding, model.Fried, model.Fixings, model.SizeAndShape) with
        | (Some Crunchy, None, None, None, Some SmallTrianglesOvalsOrRectangles) -> ItsNachos
        | (Some Crunchy, None, None, None, Some Round) -> ItsTostada
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
        | ItsTostada -> Some Tostada
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
              History = Stack<Model>()
              Language = English }

        (model, Cmd.none)

    let initWithLang lang =
        let (model, cmd) = init ()
        let model' = { model with Language = lang }
        (model', cmd)

    let updateModel msg model f =
        let tortilla = f model.Tortilla

        let timelineEntry =
            Questions.getTimelineEntry model.Language model.NextQuestion.Value msg

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
        | Restart -> initWithLang model.Language
        | GoBack ->
            if model.History.Count >= 2 then
                model.History.Pop() |> ignore
                let currentModel = model.History.Peek()
                (currentModel, Cmd.none)
            else
                initWithLang model.Language
        | ChooseLanguage lang ->
            let model' = { model with Language = lang }
            (model', Cmd.none)

module View =

    open Feliz
    open Feliz.Bulma
    open Feliz.UseElmish

    open Model
    open I18n

    let dishInfos dish =
        match dish with
        | Nachos -> ("Nachos", "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cc/Nachos1.jpg/1920px-Nachos1.jpg")
        | Tostada -> ("Tostada", "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4d/TostadasTinga.JPG/2880px-TostadasTinga.JPG")
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

    let renderResult lang dish =
        let (name, url) = dishInfos dish

        Bulma.box
            [ prop.className "result"
              prop.children
                  [ Bulma.columns
                        [ Bulma.column
                              [ Html.p (translate lang AppString.Result)
                                Html.p [ prop.className "dish"; prop.text name ]
                                Html.p "Buen provecho :)" ]
                          Bulma.column [ Html.img [ prop.src url ] ] ] ] ]

    let button (text: string) m dispatch =
        Bulma.button.button [ prop.text text; prop.onClick (fun _ -> m |> dispatch); color.isInfo ]

    let renderQuestion lang question dispatch =
        let (q, answers) = Questions.getQuestion lang question

        Html.div
            [ Html.div [ Html.strong q ]
              for (a, aMsg) in answers do
                  button a aMsg dispatch ]

    let renderTimeline lang entries =
        Bulma.box
            [ Html.strong (translate lang AppString.Choices)
              Html.unorderedList
                  [ for e in entries do
                        Html.listItem [ Html.p $"{e.Question}"; Html.strong $"{e.Answer}" ]
                        Html.br [] ] ]

    let renderQnA state dispatch =
        Bulma.card
            [ Bulma.cardContent
                  [ if Option.isSome state.Tortilla.Dish then
                        renderResult state.Language state.Tortilla.Dish.Value
                    else if Option.isSome state.NextQuestion then
                        renderQuestion state.Language state.NextQuestion.Value dispatch
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
                                      Html.span (translate state.Language AppString.PrevQuestion) ] ]
                          Bulma.button.button
                              [ prop.onClick (fun _ -> Restart |> dispatch)
                                color.isDanger
                                prop.children
                                    [ Bulma.icon [ Html.i [ prop.className "fas fa-sync" ] ]
                                      Html.span (translate state.Language AppString.Restart) ] ] ] ] ]

    let renderCard state dispatch =
        Bulma.card
            [ Bulma.cardContent
                  [ Bulma.columns
                        [ Bulma.column [ renderQnA state dispatch ]
                          Bulma.column [ renderTimeline state.Language state.Timeline ] ] ] ]

    let languaMenu dispatch =

        let setLang lang =
            match lang with
            | "Es" -> ChooseLanguage Spanish |> dispatch
            | _ -> ChooseLanguage English |> dispatch

        Bulma.field.div
            [ prop.id "language-dropdown"
              prop.children
                  [ Bulma.control.p
                        [ control.hasIconsLeft
                          prop.onChange setLang
                          prop.children
                              [ Bulma.select
                                    [ Html.option [ prop.value "En"; prop.text "En" ]
                                      Html.option [ prop.value "Es"; prop.text "Es" ] ]
                                Bulma.icon
                                    [ icon.isSmall
                                      icon.isLeft
                                      prop.children [ Html.i [ prop.className "fas fa-globe" ] ] ] ] ] ] ]

    [<ReactComponent>]
    let ViewComp () =

        let state, dispatch = React.useElmish (State.init, State.update, [||])

        [ Bulma.title "Tortilla flow"; languaMenu dispatch; renderCard state dispatch ]
        |> Html.div

open Browser.Dom
open View

Feliz.ReactDOM.render (ViewComp, document.getElementById "elmish-app")
