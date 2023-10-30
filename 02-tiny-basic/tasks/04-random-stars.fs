// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
    | StringValue of string
    | NumberValue of int
    | BoolValue of bool

type Expression =
    | Const of Value
    | Function of string * Expression list
    | Variable of string

type Command =
    | Print of Expression
    | Run
    | Goto of int
    | Assign of string * Expression
    | If of Expression * Command
    // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at
    // the console location (x, y). In C64, the actual POKE writes to a given
    // memory location, but we only use it for screen access here.
    | Clear
    | Poke of Expression * Expression * Expression

type State =
    { Program: list<int * Command>
      Variables: Map<string, Value>
      RandomGenerator: Random }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
    // Take 'value' of type 'Value', pattern match on it and print it nicely.
    match value with
    | StringValue(s) -> Console.Write(s)
    | NumberValue(num) -> Console.Write(num)
    | BoolValue(b) -> Console.Write(b)


let getLine state line =
    // Get a line with a given number from 'state.Program' (this can fail
    // if the line is not there.) You need this in the 'Goto' command case below.
    match List.tryFind (fun (ln, _) -> ln = line) state.Program with
    | Some value -> value
    | None -> failwith "Line does not exist."


let addLine state (line, cmd) =
    // Add a given line to the program state. This should overwrite
    // a previous line (if there is one with the same number) and also ensure
    // that state.Program is sorted by the line number.
    // HINT: Use List.filter and List.sortBy. Use F# Interactive to test them!
    let unsorted = (line, cmd) :: List.filter (fun (nl, _) -> nl <> line) state.Program
    let sorted = List.sortBy (fun (nl, _) -> nl) unsorted
    { state with Program = sorted }


// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args =
    match args with
    | [ NumberValue a; NumberValue b ] -> BoolValue(f a b)
    | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr =
    // Add support for 'RND(N)' which returns a random number in range 0..N-1
    // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
    // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
    // add helpers for numerical operators and binary Boolean operators to make
    // your code a bit nicer.
    match expr with
    | Const value -> value
    | Variable(name) ->
        match state.Variables.TryFind name with
        | Some value -> value
        | None -> failwith "unknown variable"
    | Function(name, parameters) ->
        match (name, parameters) with
        | ("-", fst_expr :: snd_expr :: []) ->
            let (fst, snd) = evalExpression state fst_expr, evalExpression state snd_expr

            match fst, snd with
            | (NumberValue(fstNum), NumberValue(sndNum)) -> NumberValue(fstNum - sndNum)
            | _ -> failwith "arguments are not numbers"
        | ("=", fst_expr :: snd_expr :: []) ->
            let (fst, snd) = evalExpression state fst_expr, evalExpression state snd_expr
            BoolValue(fst = snd)
        | ("RND", num_expr :: []) ->
            match evalExpression state num_expr with
            | NumberValue num ->
                let res = state.RandomGenerator.Next(num)
                NumberValue(res)
            | _ -> failwith "argument of RND is not a number"
        | (binOp, args) ->
            let evaluated_args = (List.map (fun expr -> evalExpression state expr) args)

            match binOp with
            | "<" -> binaryRelOp (<) evaluated_args
            | ">" -> binaryRelOp (>) evaluated_args
            | "||" ->
                match evaluated_args with
                | [ BoolValue a; BoolValue b ] -> BoolValue(a || b)
                | _ -> failwith "|| requires arguments to be booleans"
            | _ -> failwith "unknown binary op"

let rec runCommand state (line, cmd) =
    match cmd with
    | Run ->
        let first = List.head state.Program
        runCommand state first
    | Print(expr) ->
        // Evaluate the expression and print the resulting value here!
        printValue (evalExpression state expr)
        runNextLine state line
    | Goto(line) ->
        // Find the right line of the program using 'getLine' and call
        // 'runCommand' recursively on the found line to evaluate it.
        let newLine = getLine state line
        runCommand state newLine
    | Assign(name, value_expr) ->
        let newState =
            { state with
                Variables = state.Variables.Add(name, (evalExpression state value_expr)) }

        runNextLine newState line
    | If(cond_expr, command) ->
        let cond = evalExpression state cond_expr

        match cond with
        | BoolValue(true) -> runCommand state (line, command)
        | _ -> runNextLine state line

    // Implement two commands for screen manipulation
    | Clear ->
        Console.Clear()
        runNextLine state line
    | Poke(x, y, e) ->
        let eval expr =
            match evalExpression state expr with
            | NumberValue value -> value
            | _ -> failwith "has to be a number"

        let evals expr =
            match evalExpression state expr with
            | StringValue value -> value
            | _ -> failwith "has to be a string"

        let (xval, yval, evalue) = (eval x, eval y, evals e)
        Console.CursorLeft <- xval
        Console.CursorTop <- yval
        Console.Write(evalue)
        runNextLine state line

and runNextLine state line =
    // Find a program line with the number greater than 'line' and evalaute
    // it using 'evalExpression' (if found) or just return 'state' (if not found).
    let newLine = List.tryFind (fun (newLine, _) -> newLine > line) state.Program

    match newLine with
    | Some value -> runCommand state value
    | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------


let runInput state (line, cmd) =
    // Simulate what happens when the user enters a line of code in the
    // interactive terminal. If the 'line' number is 'Some ln', we want to
    // insert the line into the right location of the program (addLine); if it
    // is 'None', then we want to run it immediately. To make sure that
    // 'runCommand' does not try to run anything afterwards, you can pass
    // 'System.Int32.MaxValue' as the line number to it (or you could use -1
    // and handle that case specially in 'runNextLine')
    match line with
    | Some(num) -> addLine state (num, cmd)
    | None -> runCommand state (-1, cmd)


let runInputs state cmds =
    // Apply all the specified commands to the program state using 'runInput'.
    // This is a one-liner if you use 'List.fold' which has the following type:
    //   ('State -> 'T -> 'State) -> 'State -> list<'T>
    List.fold (runInput) state cmds


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.:
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [ a; b ])
let (.<) a b = Function("<", [ a; b ])
let (.>) a b = Function(">", [ a; b ])
let (.-) a b = Function("-", [ a; b ])
let (.=) a b = Function("=", [ a; b ])
let (@) s args = Function(s, args)

let empty =
    { Program = []
      Variables = Map.empty
      RandomGenerator = Random() }

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars =
    [ Some 10, Clear
      Some 20, Poke("RND" @ [ num 60 ], "RND" @ [ num 20 ], str "*")
      Some 30, Assign("I", num 100)
      Some 40, Poke("RND" @ [ num 60 ], "RND" @ [ num 20 ], str " ")
      Some 50, Assign("I", var "I" .- num 1)
      Some 60, If(var "I" .> num 1, Goto(40))
      Some 100, Goto(20)
      None, Run ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
