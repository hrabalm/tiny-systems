// ----------------------------------------------------------------------------
// 02 - "Drag down" formula expanding
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

type Sheet = Map<Address, Expr>

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr: Expr) =
    // Replace references in expression 'srcExpr' in a way that
    // corresponds to moving the expression from address (srcRow, srcCol)
    // to address (tgtRow, tgtCol). So for example, if a formula 'A1+A2' is
    // moved from 'A3' to 'B10' then it should change to 'B8+B9' (address
    // is incremented by column difference 1 and row difference 7)
    let colDiff = tgtCol - srcCol
    let rowDiff = tgtRow - srcRow

    match srcExpr with
    | Const _ -> srcExpr
    | Reference(a, b) -> Reference(a + colDiff, b + rowDiff)
    | Function(s, list) -> Function(s, List.map (fun e -> relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) e) list)


let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet: Sheet) : Sheet =
    // Expand formula at address (srcCol, srcRow) to all the cells
    // between itself and target cell at address (tgtCol, tgtRow) and
    // add the new formulas to the given sheet, returning the new sheet.
    //
    // HINT: You can use list comprehension with 'for .. in .. do' and
    // 'yield' or you can use 'List.init'. The comprehension is nicer,
    // but you need to figure out the right syntax! Once you generate
    // new cells, you can add them to the Map using List.fold (with the
    // sheet as the current state, updated in each step using Map.add).
    let targets =
        [ for col in srcCol ..tgtCol do
              for row in srcRow ..tgtRow do
                if col <> srcCol || row <> srcRow then
                  yield (col, row) ]

    let cells =
        List.map (fun addr -> addr, relocateReferences (srcCol, srcRow) addr sheet[srcCol, srcRow]) targets

    List.fold (fun sheet (address, value) -> (Map.add address value sheet)) sheet cells


// ----------------------------------------------------------------------------
// Simple recursive evaluator
// ----------------------------------------------------------------------------

let rec eval (sheet: Sheet) expr =
    // Implement simple recursive evauator!
    // * This should support functions "+" and "*" with two arguments.
    // * All other function calls should evaluate to Error.
    // * Reference to a cell that is not in 'sheet' should evaluate to Error.
    // * To evaluat Reference, get the expression from the cell and evaluate that.
    //   (we will replace this with incremental event-based code later)
    match expr with
    | Const x -> x
    | Reference x ->
        match Map.tryFind x sheet with
        | Some y -> eval sheet y
        | _ -> Error "Missing value"
    | Function(f, list) ->
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



// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s: string) =
    // Parse a cell reference such as 'A10' or 'C3'. You can assume that
    // this will always be one letter followed by a number. You can access
    // characters using "Hello".[2], convert them to integer using 'int'
    // (int 'A' returns 65, but int "123" parses the string and returns 123).
    Address(int s[0], int s[1..])



let fib =
    [ addr "A1", Const(Number 0)
      addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A1"); Reference(addr "A2") ]) ]
    |> Map.ofList
    |> expand (addr "A3") (addr "A10")

// Should return: Number 13
eval fib (Reference(addr "A8"))

// Should return: Number 21
eval fib (Reference(addr "A9"))

// Should return: Number 34
eval fib (Reference(addr "A10"))

// Should return: Error "Missing value"
eval fib (Reference(addr "A11"))


// Column 'A' is a sequence of numbers increasing by 1
// Column 'B' is the factorial of the corresponding number
// i.e.: Bn = An * B(n-1) = An * A(n-1)!
let fac =
    [ addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A2"); Const(Number 1) ])
      addr "B1", Const(Number 1)
      addr "B2", Function("*", [ Reference(addr "A2"); Reference(addr "B1") ]) ]
    |> Map.ofList
    |> expand (addr "A3") (addr "A11")
    |> expand (addr "B2") (addr "B11")

// A6 should be 5, B6 should be 120
eval fac (Reference(addr "A6"))
eval fac (Reference(addr "B6"))

// A11 should be 10, B11 should be 3628800
eval fac (Reference(addr "A11"))
eval fac (Reference(addr "B11"))
