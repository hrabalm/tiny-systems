// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
    | Run
    | Goto of int
    | Assign of string * Expression
    | If of Expression * Command
    | Clear
    | Poke of Expression * Expression * Expression
    // NOTE: Input("X") reads a number from console and assigns it to X;
    // Stop terminates the program; I also modified Print to take a list of
    // expressions instead of just one (which is what C64 supports too).
    | Print of Expression list
    | Input of string
    | Stop

type State =
    { Program: list<int * Command>
      Variables: Map<string, Value>
      Random: System.Random }

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

let binaryRelOp f args =
    match args with
    | [ NumberValue a; NumberValue b ] -> BoolValue(f a b)
    | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr =
    // We need an extra function 'MIN' that returns the smaller of
    // the two given numbers (in F#, the function 'min' does exactly this.)
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
                let res = state.Random.Next(num)
                NumberValue(res)
            | _ -> failwith "argument of RND is not a number"
        | ("MIN", fst_expr :: snd_expr :: []) ->
            let (fst, snd) = evalExpression state fst_expr, evalExpression state snd_expr

            match fst, snd with
            | (NumberValue(fstNum), NumberValue(sndNum)) -> NumberValue(min fstNum sndNum)
            | _ -> failwith "arguments are not numbers"
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
    | Print(expr_list) ->
        // Evaluate the expression and print the resulting value here!
        for expr in expr_list do
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
    // Input("X") should read a number from the console using Console.RadLine
    // and parse it as a number using Int32.TryParse (retry if the input is wrong)
    // Stop terminates the execution (you can just return the 'state'.)
    | Input(s) ->
        let mutable success = false
        let mutable state = state

        while not success do
            match Int32.TryParse(Console.ReadLine()) with
            | true, result ->
                success <- true

                state <-
                    { state with
                        Variables = state.Variables.Add(s, NumberValue(result)) }
            | _ -> ()

        runNextLine state line
    | Stop -> state

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
      Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim =
    [ Some 10, Assign("M", num 20)
      Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
      Some 30,
      Print
          [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "
            "MIN" @ [ num 5; var "M" ]
            str " MATCHES\n" ]
      Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
      Some 50, Input("P")
      Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
      Some 70, Assign("M", var "M" .- var "P")
      Some 80, If(var "M" .= num 0, Goto 200)
      Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
      Some 100,
      Print
          [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "
            "MIN" @ [ num 5; var "M" ]
            str " MATCHES\n" ]
      Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
      Some 120, Input("P")
      Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
      Some 140, Assign("M", var "M" .- var "P")
      Some 150, If(var "M" .= num 0, Goto 220)
      Some 160, Goto 20
      Some 200, Print [ str "PLAYER 1 WINS!" ]
      Some 210, Stop
      Some 220, Print [ str "PLAYER 2 WINS!" ]
      Some 230, Stop
      None, Run ]

runInputs empty nim |> ignore
