// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC
open System

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { Program : list<int * Command> 
    Variables: Map<string, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  // Take 'value' of type 'Value', pattern match on it and print it nicely.
  match value with
  | StringValue(s) ->
    Console.Write(s)
  | NumberValue(num) ->
    Console.Write(num)
  | BoolValue(b) ->
    Console.Write(b) 


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
  { state with Program = sorted}


// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr = 
  // Add support for 'Function' and 'Variable'. For now, handle just the two
  // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
  // (takes two values and returns Boolean). Note that you can test if two
  // F# values are the same using '='. It works on values of type 'Value' too.
  //
  // HINT: You will need to pass the program state to 'evalExpression' 
  // in order to be able to handle variables!
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
        | (NumberValue(fstNum), NumberValue(sndNum)) ->
          NumberValue (fstNum - sndNum)
        | _ -> failwith "arguments are not numbers"
      | ("=", fst_expr :: snd_expr :: []) ->
        let (fst, snd) = evalExpression state fst_expr, evalExpression state snd_expr
        BoolValue(fst = snd)
      | _ -> failwith "unknown fuction or invalid parameters"



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

    // TODO: Implement assignment and conditional. Assignment should run the
    // next line after setting the variable value. 'If' is a bit trickier:
    // * 'l1: IF TRUE THEN GOTO <l1>' will continue evaluating on line 'l2'
    // * 'l1: IF FALSE THEN GOTO <l1>' will continue on line after 'l1'
    // * 'l1: IF TRUE THEN PRINT "HI"' will print HI and continue on line after 'l1'
    //
    // HINT: If <e> evaluates to TRUE, you can call 'runCommand' recursively with
    // the command in the 'THEN' branch and the current line as the line number.
    | Assign(name, value_expr) ->
        let newState =
            { state with
                Variables = state.Variables.Add(name, (evalExpression state value_expr)) }

        runNextLine newState line
    | If(cond_expr, command) ->
        let cond = evalExpression state cond_expr
        match cond with
        | BoolValue(true) ->
          let newState = runCommand state (line, command)
          newState
        | _ -> runNextLine state line

and runNextLine state line = 
  // Find a program line with the number greater than 'line' and evalaute
  // it using 'evalExpression' (if found) or just return 'state' (if not found).
  let newLine =  List.tryFind (fun (newLine, _) -> newLine > line) state.Program
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

let empty = { Program = []; Variables = Map.empty } 

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
