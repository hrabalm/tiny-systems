// ----------------------------------------------------------------------------
// 03 - Reactive event-based structure
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

// Node in a dependency graph that represents a spreadsheet cell
// For each cell, we store the original expression, evalauted value
// and an event to be triggered when the value changes.
type CellNode =
    { mutable Value: Value
      mutable Expr: Expr }

// A live spreadsheet is a mapping from addresses to graph nodes
type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet: LiveSheet) expr =
    // Modify the 'Reference' case. Instead of recursively calling
    // 'eval', this should now locate the graph node and return the 'Value'
    // that is stored in the graph node!
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
        | ("*", [ x; y ]) ->
            match ((eval sheet x), (eval sheet y)) with
            | (Number xn, Number xy) -> Number(xn * xy)
            | _ -> Error "Invalid arguments"
        | _ -> Error "Unknown"



let makeNode addr (sheet: LiveSheet) (expr: Expr) : CellNode =
    // Create a dependency graph node. In this step, we just want
    // to get the same functionality as before (i.e., no event handling)
    // so evaluate the expression, store it and return the node.
    let value = eval sheet expr
    { Expr = expr; Value = value }


let makeSheet (list: (Address * Expr) list) : LiveSheet =
    // Previously, we could turn a list of mappings into a sheet just
    // by using Map.ofList. This no longer works, because we need to add
    // cells one by one (we should make sure that all cells on which the new one
    // depends are already in the sheet, but we assume examples are given
    // in a correct order). To do this, use 'List.fold' and 'makeNode'.
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

let fib =
    [ addr "A1", Const(Number 0)
      addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A1"); Reference(addr "A2") ]) ]
    |> makeSheet
    |> expand (addr "A3") (addr "A10")

// Should return: Number 13
eval fib (Reference(addr "A8"))
// Should return: Number 21
eval fib (Reference(addr "A9"))
// Should return: Number 34
eval fib (Reference(addr "A10"))
// Should return: Error "Missing value"
eval fib (Reference(addr "A11"))


let fac =
    [ addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A2"); Const(Number 1) ])
      addr "B1", Const(Number 1)
      addr "B2", Function("*", [ Reference(addr "A2"); Reference(addr "B1") ]) ]
    |> makeSheet
    |> expand (addr "A3") (addr "A11")
    |> expand (addr "B2") (addr "B11")

// Should return: Number 5
eval fac (Reference(addr "A6"))
// Should return: Number 12
eval fac (Reference(addr "B6"))

// Should return: Number 10
eval fac (Reference(addr "A11"))
// Should return: Number 3628800
eval fac (Reference(addr "B11"))
