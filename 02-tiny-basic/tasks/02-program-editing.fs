// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC
open System

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------
let printValue value = 
  // Take 'value' of type 'Value', pattern match on it and print it nicely.
  match value with
  | StringValue(s) ->
    Console.Write(s)

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

let rec evalExpression expr = 
  // Implement evaluation of expressions. The function should take 
  // 'Expression' and return 'Value'. In this step, it is trivial :-)
  match expr with
  | Const value -> value

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      // Evaluate the expression and print the resulting value here!
      printValue (evalExpression expr)
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto(line) ->
      // Find the right line of the program using 'getLine' and call 
      // 'runCommand' recursively on the found line to evaluate it.
      let newLine = getLine state line
      runCommand state newLine

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

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
