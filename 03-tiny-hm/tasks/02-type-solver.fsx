// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise
// a bit more interesting, we will implement constraint resolution
// for lists here already. This will help you in the next steps!
type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type

let rec occursCheck vcheck ty =
    // Return true of type 'ty' contains variable 'vcheck'
    match ty with
    | TyVariable(s) -> s = vcheck
    | TyList(inner_type) -> occursCheck vcheck inner_type
    | _ -> false

let rec substType (subst: Map<string, Type>) ty =
    // Apply all the specified substitutions to the type 'ty'
    // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
    match ty with
    | TyVariable(v) when subst.ContainsKey(v) -> subst.[v]
    | TyVariable(_) -> ty
    | TyList(nty) -> TyList(substType subst nty)
    | _ -> ty

let substConstrs (subst: Map<string, Type>) (cs: list<Type * Type>) =
    // Apply substitution 'subst' to all types in constraints 'cs'
    List.map (fun (e1, e2) -> substType subst e1, substType subst e2) cs


let rec solve cs =
    match cs with
    | [] -> []
    | (TyNumber, TyNumber) :: cs
    | (TyBool, TyBool) :: cs -> solve cs
    | (TyNumber, TyBool) :: cs
    | (TyBool, TyNumber) :: cs -> failwith "Cannot be solved"
    | (TyVariable(v), t) :: cs
    | (t, TyVariable(v)) :: cs ->
        if occursCheck v t then
            failwith "Cannot be solved (occurs check)"

        let constraints: (Type * Type) list = substConstrs (Map.ofList ([ (v, t) ])) cs
        let subst = solve constraints
        let t = substType (Map.ofList subst) t
        (v, t) :: (subst)
    | (TyList(t1), TyList(t2)) :: cs -> solve ((t1, t2) :: cs)
    | (TyList(_), _) :: _
    | (_, TyList(_)) :: _ -> failwith "Cannot be solved"

// TODO: Fill in the remaining cases! You can closely follow the
// example from task 1 - the logic here is exactly the same.


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
printfn
    "%A"
    (solve
        [ TyList(TyVariable("a")), TyList(TyNumber)
          TyVariable("b"), TyList(TyVariable("a")) ])
// Cannot be solved (list<'a> <> bool)
// solve
//   [ TyList(TyVariable("a")), TyVariable("b")
//     TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
printfn "%A" (solve [ TyList(TyVariable("a")), TyVariable("b"); TyVariable("b"), TyList(TyNumber) ])
// Cannot be solved ('a <> list<'a>)
solve [ TyList(TyVariable("a")), TyVariable("a") ]
