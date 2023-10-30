// ----------------------------------------------------------------------------
// 01 - Add GOTO and better PRINT for infinite loop fun!
// ----------------------------------------------------------------------------

// NOTE: You can run this using 'dotnet run' from the terminal. 
// If you want to run code in a different file, you will need to change
// the 'tinybasic.fsproj' file (which references this source file now).

// NOTE: F# code in projects is generally organized using namespaces and modules.
// Here, we declare module name for the source code in this file.
module TinyBASIC
open System

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  // NOTE: GOTO specified line number. Note that this is an integer, rather 
  // than an expression, so you cannot calculate line number dynamically. 
  // (But there are tricks to do this by direct memory access on a real C64!)
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  // TODO: Take 'value' of type 'Value', pattern match on it and print it nicely.
  match value with
  | StringValue(s) ->
    Console.Write(s)

let getLine state line =
  // TODO: Get a line with a given number from 'state.Program' (this can fail 
  // if the line is not there.) You need this in the 'Goto' command case below.
  match List.tryFind (fun (ln, _) -> ln = line) state.Program with
  | Some value -> value
  | None -> failwith "Line does not exist."

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
  // TODO: Find a program line with the number greater than 'line' and evalaute
  // it using 'evalExpression' (if found) or just return 'state' (if not found).
  let newLine =  List.tryFind (fun (newLine, _) -> newLine > line) state.Program
  match newLine with
  | Some value -> runCommand state value 
  | None -> state

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) ] }

let helloInf = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) 
      20, Goto 10 ] }

// NOTE: First try to get the following to work!
runCommand helloOnce (-1, Run) |> ignore

// NOTE: Then add 'Goto' and get the following to work!
runCommand helloInf (-1, Run) |> ignore

