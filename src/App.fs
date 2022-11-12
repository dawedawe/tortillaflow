module App

module Model =

    type Folding =
        | Roundish
        | FlatFoldedInHalf

    type Fried = bool

    type FillingOrSurrounding =
        | Empty
        | Meat
        | StripsOfMeat of bool
        | Cheese
        | Rice of bool
        | Soup
        | SauceOnTop of bool

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
        | EmptyTacoShellForParty
        | TortillaSoup
        | Fajita
        | Enchilada
        | Quesadilla
        | Burrito
        | Chimichanga

    type Question =
        | WhatCondition
        | WhatSizeAndShape
        | IsMeatInside
        | WhatsInside
        | AnyRice
        | IsFried
        | WhatFolding
        | HasStripsOfMeat
        | HasSauceOnTop

    type Model =
        { NextQuestion: Question option
          Condition: Condition option
          Folding: Folding option
          Fried: Fried option
          FillingOrSurrounding: FillingOrSurrounding List
          SizeAndShape: SizeAndShape option
          Comida: Comida option }

    type Msg =
        | ChooseCondition of Condition
        | ChooseSizeAndShap of SizeAndShape
        | ChooseIsMeatInside of bool
        | ChooseWhatsInside of FillingOrSurrounding
        | ChooseAnyRice of bool
        | ChooseIsFried of bool
        | ChooseFolding of Folding
        | ChooseHasStripsOfMeat of bool
        | ChooseSauceOnTop of bool
        | Restart

module State =

    open Elmish

    open Model

    let (|ConditionChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (None, None, [], None, None) -> Some WhatCondition
        | _ -> None

    let (|SizeAndShapeChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Crunchy, None, [], None, None) -> Some WhatSizeAndShape
        | _ -> None

    let (|MeatInsideChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Crunchy, Some Handsized, [], None, None) -> Some IsMeatInside
        | _ -> None

    let (|WhatsInsideChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Soft, None, [], None, None) -> Some WhatsInside
        | _ -> None

    let (|AnyRiceChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Soft, None, [ Meat ], None, None) -> Some AnyRice
        | _ -> None

    let (|FriedChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Soft, None, [ Meat; Rice true ], None, None) -> Some IsFried
        | (Some Soft, None, [ Meat; Rice false ], Some Roundish, None) -> Some IsFried
        | _ -> None

    let (|FoldingChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Soft, None, [ Meat; Rice false ], None, None) -> Some WhatFolding
        | _ -> None

    let (|StripsOfMeatChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Soft, None, [ Meat; Rice false ], Some FlatFoldedInHalf, None) -> Some HasStripsOfMeat
        | _ -> None

    let (|SauceChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Soft, None, [ Meat; Rice false; StripsOfMeat true ], Some FlatFoldedInHalf, None) -> Some HasSauceOnTop
        | _ -> None

    let decideNextQuestion model =
        match model with
        | ConditionChoiceNeeded q -> Some q
        | SizeAndShapeChoiceNeeded q -> Some q
        | MeatInsideChoiceNeeded q -> Some q
        | WhatsInsideChoiceNeeded q -> Some q
        | AnyRiceChoiceNeeded q -> Some q
        | FriedChoiceNeeded q -> Some q
        | FoldingChoiceNeeded q -> Some q
        | StripsOfMeatChoiceNeeded q -> Some q
        | SauceChoiceNeeded q -> Some q
        | _ -> None

    let (|ItsNachos|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Crunchy, None, None, [], Some SmallTrianglesOvalsOrRectangles) -> Some Nachos
        | _ -> None

    let (|ItsTaquito|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Crunchy, None, None, [], Some RolledUp) -> Some Taquito
        | _ -> None

    let (|ItsTaco|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Crunchy, None, None, [ Meat ], Some Handsized) -> Some Taco
        | (Some Soft, Some Roundish, Some false, [ Meat; Rice false ], _) -> Some Taco
        | (Some Soft, Some FlatFoldedInHalf, None, [ Meat; Rice false; StripsOfMeat false ], _) -> Some Taco
        | _ -> None

    let (|ItsTacoShellsForParty|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Crunchy, None, None, [ Empty ], Some Handsized) -> Some EmptyTacoShellForParty
        | _ -> None

    let (|ItsTortillaSoup|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Soft, None, None, [ Soup ], None) -> Some TortillaSoup
        | _ -> None

    let (|ItsQuesadilla|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Soft, None, None, [ Cheese ], None) -> Some Quesadilla
        | _ -> None

    let (|ItsBurito|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Soft, None, Some false, [ Meat; Rice true ], None) -> Some Burrito
        | _ -> None

    let (|ItsChimichanga|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Soft, None, Some true, [ Meat; Rice true ], None) -> Some Chimichanga
        | (Some Soft, Some Roundish, Some true, [ Meat; Rice false ], None) -> Some Chimichanga
        | _ -> None

    let (|ItsEnchilada|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Soft, Some FlatFoldedInHalf, None, [ Meat; Rice false; StripsOfMeat true; SauceOnTop true ], None) ->
            Some Enchilada
        | _ -> None

    let (|ItsFajita|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Soft, Some FlatFoldedInHalf, None, [ Meat; Rice false; StripsOfMeat true; SauceOnTop false ], None) ->
            Some Fajita
        | _ -> None

    let getComida model =
        match model with
        | ItsNachos x -> Some x
        | ItsTaquito x -> Some x
        | ItsTaco x -> Some x
        | ItsTacoShellsForParty x -> Some x
        | ItsTortillaSoup x -> Some x
        | ItsQuesadilla x -> Some x
        | ItsBurito x -> Some x
        | ItsChimichanga x -> Some x
        | ItsEnchilada x -> Some x
        | ItsFajita x -> Some x
        | _ -> None

    let init () =
        let model =
            { NextQuestion = Some WhatCondition
              Condition = None
              Folding = None
              Fried = None
              FillingOrSurrounding = List.empty
              SizeAndShape = None
              Comida = None }

        (model, Cmd.none)

    let update (msg: Msg) (model: Model) =
        match msg with
        | ChooseCondition c ->
            let model' = { model with Condition = Some c }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | ChooseSizeAndShap s ->
            let model' = { model with SizeAndShape = Some s }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | ChooseIsMeatInside b ->
            let model' =
                let toAdd = if b then Meat else Empty
                { model with FillingOrSurrounding = List.append model.FillingOrSurrounding [ toAdd ] }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | ChooseWhatsInside x ->
            let model' =
                { model with FillingOrSurrounding = List.append model.FillingOrSurrounding [ x ] }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | ChooseAnyRice b ->
            let model' =
                { model with FillingOrSurrounding = List.append model.FillingOrSurrounding [ Rice b ] }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | ChooseIsFried b ->
            let model' = { model with Fried = Some b }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)

        | ChooseFolding f ->
            let model' = { model with Folding = Some f }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | ChooseHasStripsOfMeat b ->
            let model' =
                { model with FillingOrSurrounding = List.append model.FillingOrSurrounding [ StripsOfMeat b ] }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | ChooseSauceOnTop b ->
            let model' =
                { model with FillingOrSurrounding = List.append model.FillingOrSurrounding [ SauceOnTop b ] }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | Restart -> init ()

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
        | EmptyTacoShellForParty ->
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

    let button (text: string) m dispatch =
        Bulma.button.button [ prop.text text; prop.onClick (fun _ -> m |> dispatch) ]

    let result comida =
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

    let whatConditionButtons dispatch =
        [ Html.p "What is your tortilla like?"
          button "soft" (ChooseCondition Soft) dispatch
          button "crunchy" (ChooseCondition Crunchy) dispatch ]
        |> Bulma.box

    let whatSizeAndShapeButtons dispatch =
        [ Html.p "What shape and size?"
          button "small triangles, ovals or rectangles" (ChooseSizeAndShap SmallTrianglesOvalsOrRectangles) dispatch
          button "But I can't tell, it's all rolled up!" (ChooseSizeAndShap RolledUp) dispatch
          button "The size of someone's hand I guess." (ChooseSizeAndShap Handsized) dispatch ]
        |> Bulma.box

    let isMeatInsideButtons dispatch =
        [ Html.p "Is there meat inside?"
          button "Darn tootin'! (Yes)" (ChooseIsMeatInside true) dispatch
          button "No. It's empty." (ChooseIsMeatInside false) dispatch ]
        |> Bulma.box

    let whatsInsideButtons dispatch =
        [ Html.p "What's inside?"
          button "mostly meat" (ChooseWhatsInside Meat) dispatch
          button "mostly cheese" (ChooseWhatsInside Cheese) dispatch
          button "This is a SOUP!" (ChooseWhatsInside Soup) dispatch ]
        |> Bulma.box

    let anyRiceButtons dispatch =
        [ Html.p "Any rice?"
          button "yup" (ChooseAnyRice true) dispatch
          button "negative" (ChooseAnyRice false) dispatch ]
        |> Bulma.box

    let isFriedButtons dispatch =
        [ Html.p "Is it fried?"
          button "yes" (ChooseIsFried true) dispatch
          button "gross. no." (ChooseIsFried false) dispatch ]
        |> Bulma.box

    let whatFoldingButtons dispatch =
        [ Html.p "How is it folded?"
          button "round-ish" (ChooseFolding Roundish) dispatch
          button "flat, folded in half" (ChooseFolding FlatFoldedInHalf) dispatch ]
        |> Bulma.box

    let hasStripsOfMeatButtons dispatch =
        [ Html.p "Strips of meat?"
          button "no" (ChooseHasStripsOfMeat false) dispatch
          button "yeah, actually" (ChooseHasStripsOfMeat true) dispatch ]
        |> Bulma.box

    let hasSauceOnTopButtons dispatch =
        [ Html.p "Sauce on top?"
          button "no" (ChooseSauceOnTop false) dispatch
          button "yes" (ChooseSauceOnTop true) dispatch ]
        |> Bulma.box

    [<ReactComponent>]
    let ViewComp () =

        let state, dispatch = React.useElmish (State.init, State.update, [||])

        let description =
            $"Condition {string state.Condition}"
            + "\n"
            + $"SizeAndShape {string state.SizeAndShape}"
            + "\n"
            + $"FillingOrSurrounding {string state.FillingOrSurrounding}"
            + "\n"
            + $"Fried {string state.Fried}"
            + "\n"
            + $"Folding {string state.Folding}"

        [ Bulma.title "Tortilla flow"

          if Option.isSome state.Comida then
              result state.Comida.Value
          else
              match state.NextQuestion with
              | Some WhatCondition -> whatConditionButtons dispatch
              | Some WhatSizeAndShape -> whatSizeAndShapeButtons dispatch
              | Some IsMeatInside -> isMeatInsideButtons dispatch
              | Some WhatsInside -> whatsInsideButtons dispatch
              | Some AnyRice -> anyRiceButtons dispatch
              | Some IsFried -> isFriedButtons dispatch
              | Some WhatFolding -> whatFoldingButtons dispatch
              | Some HasStripsOfMeat -> hasStripsOfMeatButtons dispatch
              | Some HasSauceOnTop -> hasSauceOnTopButtons dispatch
              | None -> ()

          Bulma.box [ Html.textarea [ prop.value description ] ]
          Bulma.box [ button "Restart" Restart dispatch ] ]
        |> Html.div

open Browser.Dom
open View

Feliz.ReactDOM.render (ViewComp, document.getElementById "elmish-app")
