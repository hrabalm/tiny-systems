// ----------------------------------------------------------------------------
// 04 - Reactive event-based computation
// ----------------------------------------------------------------------------

type Address = int * int

type Value =
    | Number of int
    | String of string
    | Error of string

type Expr =
    | Const of Value
    | Reference of Address
    | Function of string * Expr list

type CellNode =
    { mutable Value: Value
      mutable Expr: Expr
      // NOTE: Added event that will be triggered when the
      // expression and value of the node is changed.
      Updated: Event<unit> }

type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet: LiveSheet) expr =
    match expr with
    | Const x -> x
    | Reference x ->
        match Map.tryFind x sheet with
        | Some y -> y.Value
        | _ -> Error "Missing value"
    | Function(f, list) ->
        printfn "llllist: %A %A" list sheet

        match (f, list) with
        | ("+", [ x; y ]) ->
            match ((eval sheet x), (eval sheet y)) with
            | (Number xn, Number xy) -> Number(xn + xy)
            | _ -> Error "Invalid arguments"
        | ("-", [ x; y ]) ->
            match ((eval sheet x), (eval sheet y)) with
            | (Number xn, Number xy) -> Number(xn - xy)
            | _ -> Error "Invalid arguments"
        | ("*", [ x; y ]) ->
            match ((eval sheet x), (eval sheet y)) with
            | (Number xn, Number xy) -> Number(xn * xy)
            | _ -> Error "Invalid arguments"
        | ("/", [ x; y ]) ->
            match ((eval sheet x), (eval sheet y)) with
            | (Number xn, Number xy) -> Number(xn / xy)
            | _ -> Error "Invalid arguments"
        | _ -> Error "Unknown"

let rec collectReferences (expr: Expr) : Address list =
    // Collect the addresses of all references that appear in the
    // expression 'expr'. This needs to call itself recursively for all
    // arguments of 'Function' and concatenate the returned lists.
    // HINT: This looks nice if you use 'List.collect'.
    match expr with
    | Const(_) -> []
    | Reference(addr) -> [ addr ]
    | Function(_, list) -> List.collect collectReferences list


let makeNode addr (sheet: LiveSheet) expr =
    // Add handling of 'Update' events!
    //
    // * When creating a node, we need to create a new event and
    //   set it as the 'Updated' event of the returned node.
    // * We then need to define 'update' function that will be triggered
    //   when any of the cells on which this one depends change. In the
    //   function, re-evaluate the formula, set the new value and trigger
    //   our Updated event to notify other cells.
    // * Before returning, use 'collectReferences' to find all cells on which
    //   this one depends and add 'update' as the handler of their
    //   'Updated' event
    //
    let updatedEvent = new Event<unit>()

    let node =
        { Expr = expr
          Value = eval sheet expr
          Updated = updatedEvent }

    let update _ =
        node.Value <- eval sheet expr
        updatedEvent.Trigger()

    for addr in collectReferences expr do
        sheet[addr].Updated.Publish.Add(update)

    node

let updateNode addr (sheet: LiveSheet) expr =
    // For now, we ignore the fact that the new expression may have
    // different set of references than the one we are replacing.
    // So, we can just get the node, set the new expression and value
    // and trigger the Updated event!
    let node = sheet[addr]

    node.Expr <- expr
    node.Value <- eval sheet expr
    node.Updated.Trigger()


let makeSheet list =
    List.fold (fun sheet (addr, expr) -> Map.add addr (makeNode addr sheet expr) sheet) Map.empty list
// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr: Expr) =
    let colDiff = tgtCol - srcCol
    let rowDiff = tgtRow - srcRow

    match srcExpr with
    | Const _ -> srcExpr
    | Reference(a, b) -> Reference(a + colDiff, b + rowDiff)
    | Function(s, list) -> Function(s, List.map (fun e -> relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) e) list)

let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet: LiveSheet) =
    // This needs to call 'makeNode' and add the resulting node,
    // instead of just adding the expression to the map as is.
    let targets =
        [ for col in srcCol..tgtCol do
              for row in srcRow..tgtRow do
                  if col <> srcCol || row <> srcRow then
                      yield (col, row) ]

    let cells =
        List.map (fun addr -> addr, relocateReferences (srcCol, srcRow) addr sheet[srcCol, srcRow].Expr) targets

    let sheet =
        List.fold (fun sheet (addr, expr) -> Map.add addr (makeNode addr sheet expr) sheet) sheet cells

    sheet

// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s: string) = Address(int s[0], int s[1..])

// Simple spreadsheet that performs conversion between Celsius and Fahrenheit
// To convert F to C, we put value in F into B1 and read the result in C1
// To convert C to F, we put value in C into B2 and read the result in C2
let tempConv =
    [ addr "A1", Const(String "F to C")
      addr "B1", Const(Number 0)
      addr "C1",
      Function(
          "/",
          [ Function("*", [ Function("-", [ Reference(addr "B1"); Const(Number 32) ]); Const(Number 5) ])
            Const(Number 9) ]
      )
      addr "A2", Const(String "C to F")
      addr "B2", Const(Number 0)
      addr "C2",
      Function(
          "+",
          [ Function("/", [ Function("*", [ Reference(addr "B2"); Const(Number(9)) ]); Const(Number 5) ])
            Const(Number 32) ]
      ) ]
    |> makeSheet

// Fahrenheit to Celsius conversions

// Should return: -17
updateNode (addr "B1") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C1"))
// Should return: 0
updateNode (addr "B1") tempConv (Const(Number 32))
eval tempConv (Reference(addr "C1"))
// Should return: 37
updateNode (addr "B1") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C1"))

// Celsius to Fahrenheit conversions

// Should return: 32
updateNode (addr "B2") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C2"))
// Should return: 212
updateNode (addr "B2") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C2"))
// Should return: 100
updateNode (addr "B2") tempConv (Const(Number 38))
eval tempConv (Reference(addr "C2"))
