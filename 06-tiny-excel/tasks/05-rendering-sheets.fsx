// ----------------------------------------------------------------------------
// 05 - Rendering sheets as HTML
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

let rec collectReferences expr =
    match expr with
    | Const(_) -> []
    | Reference(addr) -> [ addr ]
    | Function(_, list) -> List.collect collectReferences list

let makeNode addr (sheet: LiveSheet) expr =
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
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v: Value) : string =
    // Turn the given value into a string representing HTML
    // You can use the following to create an error string in red.
    match v with
    | Number(x) -> sprintf "<span>%d</span>" x
    | String(s) -> sprintf "<span>%s</span>" s
    | Error(e) -> sprintf "<span class='e'>%s</span>" e

let display (sheet: LiveSheet) =
    // Find the greates row and column index
    let maxCol = Map.fold (fun a (col, _) _ -> (max a col)) 0 sheet
    let maxRow = Map.fold (fun a (_, row) _ -> (max a row)) 0 sheet

    let f = Path.GetTempFileName() + ".html"
    use wr = new StreamWriter(File.OpenWrite(f))

    wr.Write(
        """<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; } 
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; } 
        th { background:#606060; color:white; } 
      </style>
    </head><body><table>"""
    )

    // Write column headings
    wr.Write("<tr><th></th>")

    for col in 1..maxCol do
        wr.Write(sprintf "<th>%c</th>" (char (int 'A' + col - 1)))

    wr.Write("</tr>")

    // Write row headings and data
    for row in 1..maxRow do
        wr.Write($"<tr><th>{row}</th>")

        for col in 1..maxCol do
            let cell = sheet.TryFind((col, row))

            let pretty =
                match cell with
                | Some(x) -> displayValue x.Value
                | _ -> ""

            wr.Write($"<td>{pretty}</td>")

        wr.Write("</tr>")

    wr.Write("</table></body></html>")
    wr.Close()
    Process.Start(f)


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s: string) =
    Address(int s[0] - (int 'A') + 1, int s[1..])

// NOTE: Let's visualize the Fibbonacci spreadsheet from Step 2!
let fib =
    [ addr "A1", Const(Number 0)
      addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A1"); Reference(addr "A2") ]) ]
    |> makeSheet
    |> expand (addr "A3") (addr "A10")

display fib

// NOTE: Let's visualize the Factorial spreadsheet from Step 2!
let fac =
    [ addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A2"); Const(Number 1) ])
      addr "B1", Const(Number 1)
      addr "B2", Function("*", [ Reference(addr "A2"); Reference(addr "B1") ]) ]
    |> makeSheet
    |> expand (addr "A3") (addr "A11")
    |> expand (addr "B2") (addr "B11")

display fac

// NOTE: Let's visualize the Temp convertor spreadsheet from Step 4!
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

display tempConv
