// ----------------------------------------------------------------------------
// 01 - Complete the simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
  | Zero
  | Succ of Number
  | Variable of string


// NOTE: The four functions below currently return a wrong 
// result, but one that makes the code run. As you implement
// them (one by one), the tests should graudally start working.


let rec occursCheck (v:string) (n:Number) = 
  // Check if variable 'v' appears anywhere inside 'n'
  match n with
  | Zero -> false
  | Variable(s) -> s = v
  | Succ(n) -> occursCheck v n

let rec substite (v:string) (subst:Number) (n:Number) =
  // Replace all occurrences of variable 'v' in the
  // number 'n' with the replacement number 'subst'
  match n with
  | Zero -> Zero
  | Succ(x) -> Succ(substite v subst x)
  | Variable(v2) when v2 = v -> subst
  | Variable(_) -> n


let substituteConstraints (v:string) (subst:Number) (constraints:list<Number * Number>) = 
  // Substitute 'v' for 'subst' (use 'substitute') in 
  // all numbers in all the constraints in 'constraints'
  List.map (fun (e1, e2) -> substite v subst e1, substite v subst e2) constraints

let substituteAll (subst:list<string * Number>) (n:Number) =
  // Perform all substitutions 
  // specified  in 'subst' on the number 'n'
  List.fold (fun n (subst) -> substite (fst subst) (snd subst) n) n subst

let rec solve constraints = 
  match constraints with 
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ -> 
      failwith "Cannot be solved"
  | (n, Variable v)::constraints | (Variable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substituteConstraints v n constraints
      let subst = solve constraints
      let n = substituteAll subst n
      (v, n)::subst

// Should work: x = Zero
printfn "%A" (
  solve [ Succ(Variable "x"), Succ(Zero) ]
)
// Should faild: S(Z) <> Z
// printfn "%A" (
// solve 
//   [ Succ(Succ(Zero)), Succ(Zero) ]
// )
// Not done: Need to substitute x/Z in S(x)
// printfn "%A" (
// solve 
//   [ Succ(Variable "x"), Succ(Zero)
//     Variable "y", Succ(Variable "x") ]
// )

// Not done: Need to substitute z/Z in S(S(z))
printfn "%A" (
solve 
  [ Variable "x", Succ(Succ(Variable "z"))
    Succ(Variable "z"), Succ(Zero) ]
)

// Not done: Need occurs check
printfn "%A" (
solve
  [ Variable "x", Succ(Variable "x") ]
)
