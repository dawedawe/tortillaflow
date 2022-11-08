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

    let view model dispatch =

        let description = string model.Condition + "\n" + string model.SizeAndShape

        [ Bulma.textarea [ prop.value description ]

          Bulma.button.button
              [ prop.text "go with a soft tortilla"
                prop.onClick (fun _ -> Msg.ChooseCondition Condition.Soft |> dispatch) ]
          Bulma.button.button
              [ prop.text "go with a crunchy tortilla"
                prop.onClick (fun _ -> Msg.ChooseCondition Condition.Crunchy |> dispatch) ]
          Bulma.button.button
              [ prop.text "small triangles, ovals or rectangles"
                prop.onClick (fun _ -> Msg.ChooseSizeAndShap SizeAndShape.SmallTrianglesOvalsOrRectangles |> dispatch) ]
          Bulma.button.button
              [ prop.text "handsized"
                prop.onClick (fun _ -> Msg.ChooseSizeAndShap SizeAndShape.Handsized |> dispatch) ]
          Bulma.button.button
              [ prop.text "rolled up"
                prop.onClick (fun _ -> Msg.ChooseSizeAndShap SizeAndShape.RolledUp |> dispatch) ] ]
        |> Fable.React.Helpers.ofList

open State
open View

Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.withConsoleTrace
|> Program.run
