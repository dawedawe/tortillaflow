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
        | Crunchy of SizeAndShape * FillingOrSurrounding
        | Soft

    type Comida =
        | Tortilla of Condition
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

    type Model = { State: Comida }

    type Msg =
        | AddCheese
        | AddSalad

module State =

    open Model

    let init () =
        let model = { State = Comida.Tortilla Condition.Soft }
        (model, Cmd.none)

    let update (msg: Msg) (model: Model) =
        match msg with
        | AddCheese -> { model with State = Fajita }, Cmd.none
        | AddSalad -> { model with State = Burito }, Cmd.none

module View =

    open Feliz
    open Feliz.Bulma
    open Model

    let view model dispatch =
        [ Bulma.textarea [ prop.value (string model.State) ]

          Bulma.button.button
              [ prop.text "change food with cheese"
                prop.onClick (fun e -> dispatch Msg.AddCheese) ]
          Bulma.button.button
              [ prop.text "change food with salad"
                prop.onClick (fun e -> dispatch Msg.AddSalad) ] ]
        |> Fable.React.Helpers.ofList

open State
open View

Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.withConsoleTrace
|> Program.run
