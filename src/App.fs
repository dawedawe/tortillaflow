module App

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

open Elmish
open Elmish.React

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

    type Model =
        { Condition: Condition option
          Folding: Folding option
          Fried: Fried option
          FillingOrSurrounding: FillingOrSurrounding option
          SizeAndShape: SizeAndShape option
          Comida: Comida option }

    type Msg =
        | ChooseCondition of Condition
        | ChooseSizeAndShap of SizeAndShape

module State =

    open Model

    let init () =
        let model =
            { Condition = None
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
            (model', Cmd.none)
        | ChooseSizeAndShap s ->
            let model' = { model with SizeAndShape = Some s }
            (model', Cmd.none)
        | _ -> System.NotImplementedException() |> raise

module View =

    open Feliz
    open Feliz.Bulma
    open Model

    let button (text: string) m dispatch =
        Bulma.button.button [ prop.text text; prop.onClick (fun _ -> m |> dispatch) ]

    let result comida =
        Bulma.textarea [ prop.value $"{string comida}!!!\nBuen provecho :)" ]

    let (|ConditionChoiceNeeded|_|) (model, dispatch) =
        if Option.isNone model.Condition then
            [ button "go with a soft tortilla" (ChooseCondition Soft) dispatch
              button "go with a crunchy tortilla" (ChooseCondition Crunchy) dispatch ]
            |> Fable.React.Helpers.ofList
            |> Some
        else
            None

    let (|SizeAndShapeChoiceNeeded|_|) (model, dispatch) =
        match (model.Condition, model.SizeAndShape) with
        | (Some Crunchy, None) ->
            [ button "small triangles, ovals or rectangles" (ChooseSizeAndShap SmallTrianglesOvalsOrRectangles) dispatch
              button "But I can't tell, it's all rolled up!" (ChooseSizeAndShap RolledUp) dispatch
              button "The size of someone's hand I guess." (ChooseSizeAndShap Handsized) dispatch ]
            |> Fable.React.Helpers.ofList
            |> Some
        | _ -> None

    let (|ItsNachos|_|) model =
        match model.Condition, model.SizeAndShape with
        | Some Crunchy, Some SmallTrianglesOvalsOrRectangles -> result Nachos |> Some
        | _ -> None

    let (|ItsTaquito|_|) model =
        match model.Condition, model.SizeAndShape with
        | Some Crunchy, Some RolledUp -> result Taquito |> Some
        | _ -> None

    let view model dispatch =

        let description = string model.Condition + "\n" + string model.SizeAndShape

        [ Bulma.textarea [ prop.value description ]

          match (model, dispatch) with
          | ConditionChoiceNeeded buttons -> buttons
          | _ -> ()

          match (model, dispatch) with
          | SizeAndShapeChoiceNeeded buttons -> buttons
          | _ -> ()

          match model with
          | ItsNachos n -> n
          | ItsTaquito n -> n
          | _ -> () ]
        |> Fable.React.Helpers.ofList

open State
open View

Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.withConsoleTrace
|> Program.run
