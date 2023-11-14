// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | If of Expression * Expression * Expression
    | Variable of string
    // NOTE: Added three more kinds of expression from TinyML
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type
    // NOTE: Added type for functions (of single argument)
    | TyFunction of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------
let rec occursCheck vcheck ty =
    // Return true of type 'ty' contains variable 'vcheck'
    match ty with
    | TyVariable(s) -> s = vcheck
    | TyList(inner_type) -> occursCheck vcheck inner_type
    | TyFunction(t1, t2) -> (occursCheck vcheck t1) && (occursCheck vcheck t2)
    | _ -> false

let rec substType (subst: Map<string, Type>) ty =
    // Apply all the specified substitutions to the type 'ty'
    // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
    match ty with
    | TyVariable(v) when subst.ContainsKey(v) -> subst.[v]
    | TyVariable(_) -> ty
    | TyList(nty) -> TyList(substType subst nty)
    | TyFunction(t1, t2) -> TyFunction(substType subst t1, substType subst t2)
    | _ -> ty

let substConstrs (subst: Map<string, Type>) (cs: list<Type * Type>) =
    // Apply substitution 'subst' to all types in constraints 'cs'
    List.map (fun (e1, e2) -> substType subst e1, substType subst e2) cs


let rec solve cs =
    // TODO: Add case matching TyFunction(ta1, tb1) and TyFunction(ta2, tb2)
    // This generates two new constraints, equating the argument/return types.
    match cs with
    | [] -> []
    | (TyNumber, TyNumber) :: cs
    | (TyBool, TyBool) :: cs -> solve cs
    | (TyNumber, TyBool) :: _
    | (TyBool, TyNumber) :: _ -> failwith "Cannot be solved"
    | (TyVariable(v), t) :: cs
    | (t, TyVariable(v)) :: cs ->
        if occursCheck v t then
            failwith "Cannot be solved (occurs check)"

        let constraints: (Type * Type) list = substConstrs (Map.ofList ([ (v, t) ])) cs
        let subst = solve constraints
        let t = substType (Map.ofList subst) t
        (v, t) :: (subst)
    | (TyList(t1), TyList(t2)) :: cs -> solve ((t1, t2) :: cs)
    | (TyFunction(ta1, tb1), TyFunction(ta2, tb2)) :: cs -> solve ((ta1, ta2) :: (tb1, tb2) :: cs)
    | (TyList(_), _) :: _
    | (_, TyList(_)) :: _ -> failwith "Cannot be solved"

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
let newTyVariable =
    let mutable n = 0

    fun () ->
        n <- n + 1
        TyVariable(sprintf "_a%d" n)

let rec generate (ctx: TypingContext) e =
    match e with
    | Constant _ -> TyNumber, []
    | Binary("+", e1, e2) -> failwith "implemented in step 3"
    | Binary("=", e1, e2) -> failwith "implemented in step 3"
    | Binary(op, _, _) -> failwith "implemented in step 3"
    | Variable v -> failwith "implemented in step 3"
    | If(econd, etrue, efalse) -> failwith "implemented in step 3"

    | Let(v, e1, e2) ->
        // TODO: Generate type & constraints for 'e1' first and then
        // add the generated type to the typing context for 't2'.
        failwith "not implemented"

    | Lambda(v, e) ->
        let targ = newTyVariable ()
        // TODO: We do not know what the type of the variable 'v' is, so we
        // generate a new type variable and add that to the 'ctx'. The
        // resulting type will be 'TyFunction' with 'targ' as argument type.
        failwith "not implemented"

    | Application(e1, e2) ->
        // TODO: Tricky case! We cannot inspect the generated type of 'e1'
        // to see what the argument/return type of the function is. Instead,
        // we have to generate a new type variable and add a constraint.
        failwith "not implemented"


// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

// Run both of the phases and return the resulting type
let infer e =
    let typ, constraints = generate Map.empty e
    let subst = solve constraints
    let typ = substType (Map.ofList subst) typ
    typ


// NOTE: Using the above, you will end up with ugly random type variable
// names like '_a4' etc. You can improve this by collecting all the type
// variable names that appear in a type and substituting them with a
// list of nice names. Useful bit of code to generate the substitution is:
//
//   Map.ofList [ for i, n in Seq.indexed ["_a4"; "_a5"] ->
//     n, string('a' + char i) ]
//
// You would still need to write code to collect all type variables in a type.


// let x = 10 in x = 10
Let("x", Constant 10, Binary("=", Variable "x", Constant 10)) |> infer

// let f = fun x -> x*2 in (f 20) + (f 1)
Let(
    "f",
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Binary("+", Application(Variable("f"), Constant(20)), Application(Variable("f"), Constant(1)))
)
|> infer

// fun x f -> f (f x)
Lambda("x", Lambda("f", Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer

// fun f -> f f
// This does not type check due to occurs check
Lambda("f", Application(Variable "f", Variable "f")) |> infer

// fun f -> f 1 + f (2 = 3)
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
Lambda(
    "f",
    Binary("+", Application(Variable "f", Constant 1), Application(Variable "f", Binary("=", Constant 2, Constant 3)))
)
|> infer
