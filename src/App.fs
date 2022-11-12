module App

module Model =

    type Folding =
        | Roundish
        | FlatFoldedInHalf

    type Fried = bool

    type FillingOrSurrounding =
        | Empty
        | Meat
        | Cheese
        | Rice
        | Soup

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
        | ChimiChanga
        | Burito
        | Chimichanga

    type Question =
        | WhatCondition
        | WhatSizeAndShape
        | IsMeatInside
        | WhatsInside

    type Model =
        { NextQuestion: Question option
          Condition: Condition option
          Folding: Folding option
          Fried: Fried option
          FillingOrSurrounding: FillingOrSurrounding option
          SizeAndShape: SizeAndShape option
          Comida: Comida option }

    type Msg =
        | ChooseCondition of Condition
        | ChooseSizeAndShap of SizeAndShape
        | ChooseIsMeatInside of bool
        | ChooseWhatsInside of FillingOrSurrounding
        | Restart

module State =

    open Elmish

    open Model

    let (|ConditionChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (None, None, None, None, None) -> Some WhatCondition
        | _ -> None

    let (|SizeAndShapeChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Crunchy, None, None, None, None) -> Some WhatSizeAndShape
        | _ -> None

    let (|MeatInsideChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Crunchy, Some Handsized, None, None, None) -> Some IsMeatInside
        | _ -> None

    let (|WhatsInsideChoiceNeeded|_|) model =
        match (model.Condition, model.SizeAndShape, model.FillingOrSurrounding, model.Folding, model.Fried) with
        | (Some Soft, None, None, None, None) -> Some WhatsInside
        | _ -> None

    let decideNextQuestion model =
        match model with
        | ConditionChoiceNeeded q -> Some q
        | SizeAndShapeChoiceNeeded q -> Some q
        | MeatInsideChoiceNeeded q -> Some q
        | WhatsInsideChoiceNeeded q -> Some q
        | _ -> None

    let (|ItsNachos|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Crunchy, None, None, None, Some SmallTrianglesOvalsOrRectangles) -> Some Nachos
        | _ -> None

    let (|ItsTaquito|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Crunchy, None, None, None, Some RolledUp) -> Some Taquito
        | _ -> None

    let (|ItsTaco|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Crunchy, None, None, Some Meat, Some Handsized) -> Some Taco
        | (Some Crunchy, None, None, Some Empty, Some Handsized) -> Some EmptyTacoShellForParty
        | _ -> None

    let (|ItsTortillaSoup|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Soft, None, None, Some Soup, None) -> Some TortillaSoup
        | _ -> None

    let (|ItsQuesadilla|_|) model =
        match (model.Condition, model.Folding, model.Fried, model.FillingOrSurrounding, model.SizeAndShape) with
        | (Some Soft, None, None, Some Cheese, None) -> Some Quesadilla
        | _ -> None

    let getComida model =
        match model with
        | ItsNachos x -> Some x
        | ItsTaquito x -> Some x
        | ItsTaco x -> Some x
        | ItsTortillaSoup x -> Some x
        | ItsQuesadilla x -> Some x
        | _ -> None

    let init () =
        let model =
            { NextQuestion = Some WhatCondition
              Condition = None
              Folding = None
              Fried = None
              FillingOrSurrounding = None
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
                { model with FillingOrSurrounding = if b then Some Meat else Some Empty }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | ChooseWhatsInside x ->
            let model' = { model with FillingOrSurrounding = Some x }

            let model'' =
                { model' with
                    NextQuestion = decideNextQuestion model'
                    Comida = getComida model' }

            (model'', Cmd.none)
        | Restart -> init ()
        | _ -> System.NotImplementedException() |> raise

module View =

    open Feliz
    open Feliz.Bulma
    open Feliz.UseElmish

    open Model

    let button (text: string) m dispatch =
        Bulma.button.button [ prop.text text; prop.onClick (fun _ -> m |> dispatch) ]

    let result comida =
        Bulma.textarea [ prop.value $"{string comida}!!!\nBuen provecho :)" ]

    let whatConditionButtons dispatch =
        [ button "soft" (ChooseCondition Soft) dispatch
          button "crunchy" (ChooseCondition Crunchy) dispatch ]
        |> Html.div

    let whatSizeAndShapeButtons dispatch =
        [ button "small triangles, ovals or rectangles" (ChooseSizeAndShap SmallTrianglesOvalsOrRectangles) dispatch
          button "But I can't tell, it's all rolled up!" (ChooseSizeAndShap RolledUp) dispatch
          button "The size of someone's hand I guess." (ChooseSizeAndShap Handsized) dispatch ]
        |> Html.div

    let isMeatInsideButtons dispatch =
        [ button "Darn tootin'! (Yes)" (ChooseIsMeatInside true) dispatch
          button "No. It's empty." (ChooseIsMeatInside false) dispatch ]
        |> Html.div

    let whatsInsideButtons dispatch =
        [ button "mostly meat" (ChooseWhatsInside Meat) dispatch
          button "mostly cheese" (ChooseWhatsInside Cheese) dispatch
          button "This is a SOUP!." (ChooseWhatsInside Soup) dispatch ]
        |> Html.div

    let (|ItsNachos|_|) model =
        match model.Condition, model.SizeAndShape with
        | Some Crunchy, Some SmallTrianglesOvalsOrRectangles -> result Nachos |> Some
        | _ -> None

    let (|ItsTaquito|_|) model =
        match model.Condition, model.SizeAndShape with
        | Some Crunchy, Some RolledUp -> result Taquito |> Some
        | _ -> None

    let (|ItsTaco|_|) model =
        match model.Condition, model.SizeAndShape, model.FillingOrSurrounding with
        | Some Crunchy, Some Handsized, Some Meat -> result Taco |> Some
        | Some Crunchy, Some Handsized, Some Empty -> result EmptyTacoShellForParty |> Some
        | _ -> None

    [<ReactComponent>]
    let ViewComp() =

        let state, dispatch = React.useElmish(State.init, State.update, [| |])

        let description =
            string state.Condition
            + "\n"
            + string state.SizeAndShape
            + "\n"
            + string state.FillingOrSurrounding

        [ Html.textarea [ prop.value description ]

          if Option.isSome state.Comida then
              result state.Comida
          else
              match state.NextQuestion with
              | Some WhatCondition -> whatConditionButtons dispatch
              | Some WhatSizeAndShape -> whatSizeAndShapeButtons dispatch
              | Some IsMeatInside -> isMeatInsideButtons dispatch
              | Some WhatsInside -> whatsInsideButtons dispatch
              | _ -> ()
          Html.br []
          button "Restart" Restart dispatch ]
        |> Html.div

open Browser.Dom
open View

Feliz.ReactDOM.render (ViewComp, document.getElementById "elmish-app")
