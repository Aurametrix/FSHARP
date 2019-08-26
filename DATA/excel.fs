type Position = char * int

type Expr = 
  | Number of int
  | Reference of Position
  | Binary of Expr * char * Expr

type Sheet = Map<Position, string>

val parse : string -> Expr option
val evaluate : Expr * Sheet -> int option

 
type State = (Record capturing the state)
type Event = (Union listing possible events)

val update : State -> Event -> State
val view : State -> (Event -> unit) -> Html


type Event =
  | UpdateValue of Position * string
  | StartEdit of Position

type State =
  { Rows : int list
    Cols : char list
    Active : Position option
    Cells : Sheet }
    
    
let update msg state = 
  match msg with 
  | StartEdit(pos) ->
      { state with Active = Some pos }, Cmd.none

  | UpdateValue(pos, value) ->
      let newCells = Map.add pos value state.Cells
      { state with Cells = newCells }, Cmd.none
      
      
      let view state trigger =
  table [] [
    thead [] [ 
      tr [] [
        yield th [] []
        for col in state.Cols -> th [] [ str (string col) ]
      ] 
    ]
    tbody [] [
      for row in state.Rows -> tr [] [
        yield th [] [ str (string row) ]
        for col in state.Cols -> renderCell trigger (col, row) state
      ]
    ]
  ]
  
  
  let renderCell trigger pos state =
  if state.Active = Some pos then
    let text = Map.tryFind pos state.Cells 
    renderEditor trigger pos (defaultArg text "")
  else
    match Map.tryFind pos state.Cells with 
    | Some input -> 
        let result = 
          parse input
          |> Option.bind (evaluate Set.empty state.Cells) 
          |> Option.map string
        renderView trigger pos result
    | _ -> renderView trigger pos (Some "")
    
    
    let renderView trigger pos (value:option<_>) = 
  let color = if value.IsNone then "#ffb0b0" else "white"
  td 
    [ Style [Background color] 
      OnClick (fun _ -> trigger(StartEdit(pos)) ) ] 
    [ str (defaultArg value "#ERR") ]

let renderEditor trigger pos value =
  td [ Class "selected"] [ 
    input [
      AutoFocus true
      OnInput (fun e -> trigger(UpdateValue(pos, e.target?value)))
      Value value ]
  ]
  
  
  let initial () = 
  { Cols = ['A' .. 'K']
    Rows = [1 .. 15]
    Active = None
    Cells = Map.empty },
  Cmd.Empty    
 
Program.mkProgram initial update view
|> Program.withReact "main"
|> Program.run


let rec evaluate cells expr = 
  match expr with
  | Number num -> 
      num 

  | Binary(l, op, r) -> 
      let ops = dict [ '+', (+); '-', (-); '*', (*); '/', (/) ]
      let l, r = evaluate cells l, evaluate cells r
      ops.[op] l r

  | Reference pos -> 
      let parsed = parse (Map.find pos cells)
      evaluate cells (Option.get parsed)
      
      
  let rec evaluate visited cells expr = 
  match expr with
  | Number num -> 
      Some num

  | Binary(l, op, r) -> 
      let ops = dict [ '+', (+); '-', (-); '*', (*); '/', (/) ]
      evaluate visited cells l |> Option.bind (fun l ->
        evaluate visited cells r |> Option.map (fun r ->
          ops.[op] l r ))

  | Reference pos when Set.contains pos visited ->
      None

  | Reference pos -> 
      Map.tryFind pos cells |> Option.bind (fun value ->
        parse value |> Option.bind (fun parsed ->
          evaluate (Set.add pos visited) cells parsed))    
