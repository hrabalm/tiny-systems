// ----------------------------------------------------------------------------
// 06 - Lazy search and support for lists
// ----------------------------------------------------------------------------

type Term =
    | Atom of string
    | Variable of string
    | Predicate of string * Term list

type Clause = { Head: Term; Body: Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------
let rec substitute (subst: Map<string, Term>) term =
    // Replace all variables that appear in 'term'
    // with the replacement specified by 'subst.[var]'.
    // You can assume the terms in 'subst' do not contain
    // any of the variables that we want to replace.
    match term with
    | Atom s -> Atom s
    | Variable s -> if subst.ContainsKey(s) then subst.[s] else Variable s
    | Predicate(n, l) -> Predicate(n, List.map (substitute subst) l)

let substituteSubst (newSubst: Map<string, Term>) (subst: list<string * Term>) =
    // Apply the substitution 'newSubst' to all the terms
    // in the existing substitiution 'subst'. (We represent one
    // as a map and the other as a list of pairs, which is a bit
    // inelegant, but it makes calling this function easier later.)
    List.map (fun (n, l) -> (n, substitute newSubst l)) subst

let substituteTerms subst (terms: list<Term>) =
    // Apply substitution 'subst' to all the terms in 'terms'
    List.map (substitute subst) terms


let rec unifyLists l1 l2 =
    // Modify the implementation to use 'substituteTerms' and 'substituteSubst'.
    //
    // Let's say that your code calls 'unify h1 h2' to get a substitution 's1'
    // and then it calls 'unifyLists t1 t2' to get a substitution 's2' and then
    // it returns a concatentated list 's1 @ s2'. Modify the code so that:
    //
    // (1) The substitution 's1' is aplied to 't1' and 't2' before calling 'unifyLists'
    // (2) The substitution 's2' is applied to all terms in substitution 's1' before returning
    //
    // You can look at your ML type inference code. The structure is very similar!
    match l1, l2 with
    | [], [] ->
        // Succeeds, but returns an empty substitution
        Some []
    | h1 :: t1, h2 :: t2 ->
        // Unify 'h1' with 'h2' using 'unify' and
        // 't1' with 't2' using 'unifyLists'. If both
        // succeed, return the generated joint substitution!
        let s1 = unify h1 h2

        match s1 with
        | Some l1 ->
            let s2 =
                unifyLists (substituteTerms (Map.ofList l1) t1) (substituteTerms (Map.ofList l1) t2)

            match s2 with
            | Some l2 ->
                let left = (substituteSubst (Map.ofList l2) l1)
                Some(left @ l2)
            | None -> None

        | None -> None
    | _ ->
        // Lists cannot be unified
        None

and unify t1 t2 =
    match t1, t2 with
    // * For matching atoms, return empty substitution
    | Atom x, Atom y when x = y -> Some []
    // * For matching predicates, return the result of 'unifyLists'
    | Predicate(n1, l1), Predicate(n2, l2) when n1 = n2 -> unifyLists l1 l2
    // * For variable and any term, return a new substitution
    | Variable(n1), t
    | t, Variable(n1) -> Some [ (n1, t) ]
    // * For anything else, return None (failed to unify)
    | _ -> None

// ----------------------------------------------------------------------------
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term =
    // Write an active pattern to recognize numbers in the form used below.
    // If the term is 'Atom("zero")' return Some(0).
    // If the term is 'Predicate("succ", [n])' where 'n' is itself
    // a term representing number, return the number value +1.
    match term with
    | Atom("zero") -> Some(0)
    | Predicate("succ", [ n ]) ->
        match n with
        | Number m -> Some(m + 1)
        | _ -> None
    | _ -> None

let rec (|List|_|) term : option<list<Term>> =
    // If the term represents a list, this should return the
    // elements of the list collected in an ordinary F# list.
    // If the term is 'Atom("empty")' return Some([])
    // If the term is 'Predicate("cons", [h; tl])' where 'tl' is itself
    // a term representing a list 'l', return Some(h::l).
    match term with
    | Atom("empty") -> Some([])
    | Predicate("cons", [ h; tl ]) ->
        match tl with
        | List l -> Some(h :: l)
        | _ -> None
    | _ -> None

let rec formatTerm term =
    match term with
    // Simple cases for number, atom and variable are done already...
    | Number n -> string n
    | Atom s -> s
    | Variable v -> v
    | List items -> "[" + (String.concat ", " (List.map formatTerm items)) + "]"
    | Predicate(p, items) ->
        // format all arguments recursively using 'formatTerm'
        // You can then concatenate the arguments using 'String.concat'
        let formatted = List.map formatTerm items
        p + "(" + (String.concat ", " formatted) + ")"

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------
let nextNumber =
    let mutable n = 0

    fun () ->
        n <- n + 1
        n


let rec freeVariables term =
    // Return a list of all variables that appear in 'term'
    // (this may contain duplicates, we will eliminate them below)
    // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
    match term with
    | Atom s -> []
    | Variable v -> [ v ]
    | Predicate(n, l) -> List.collect (freeVariables) l


let withFreshVariables (clause: Clause) : Clause =
    // Get a list of distinct variables in the clause (using
    // 'freeVariables' and 'List.distinct'), generate a substitution
    // that append a number 'n' obtained by 'nextNumber()' to the end
    // of all the variable names, and apply the substitutions to the
    // head and body of the clause.
    //
    // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
    // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
    //
    // This may not be correct if the user-provided names of variables
    // had numbers in them in a certain format, but that's OK for now!
    let n = nextNumber ()

    let variables =
        List.distinct (freeVariables clause.Head @ List.collect freeVariables clause.Body)

    let subst =
        Map.ofList (List.map (fun name -> (name, Variable(name + n.ToString()))) variables)

    { Head = (substitute subst clause.Head)
      Body = (substituteTerms subst clause.Body) }


let query (program: list<Clause>) (query: Term) : list<Clause * list<string * Term>> =
    // Return all clauses from 'program' whose 'Head' can be
    // unified with the specified 'query' and return the resulting
    // substitutions. Before unifying, rename variables in the program
    // rule using 'withFreshVariables'. You can do this using 'List.choose'
    // or by using list comprehension.
    //
    // The return type of this is a list of tuples consisting of the matching
    // clause and a substitution (list<string * Term>). Calling 'unify'
    // gives you 'option<list<string * Term>>', so you need to pattern match
    // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.
    let renamedProgram = List.map withFreshVariables program

    let choose (clause: Clause) =
        let subst = unify clause.Head query

        match subst with
        | Some s -> Some(clause, s)
        | None -> None

    List.choose choose renamedProgram

let rec solve program subst goals : seq<list<string * Term>> =
    seq {
        // We want to change this function to return a lazy sequence
        // of all possible substitutions solving the problem. I already
        // wrapped the code in 'seq { .. }' block for you. Change the rest
        // to recursively call 'solve' using 'yield!' and return new
        // solutions using 'yield' (replacing the printing code).
        match goals with
        | g :: goals ->
            let matches = query program g

            for clause, newSubst in matches do
                let newGoals = substituteTerms (Map.ofList newSubst) (goals @ clause.Body)
                let subst = newSubst @ (substituteSubst (Map.ofList newSubst) subst)
                yield! solve program subst newGoals

        | [] -> yield subst
    }

let run program query =
    let vars = Set.ofSeq (freeVariables query)

    for subst in solve program [] [ query ] do
        // To avoid cluttered output, we want to only print assignment
        // for variables that appear in the original query (and skip all
        // variables generated by the various internal matches). You can do
        // this here by iterating over variables and printing them only if
        // they are included in 'vars' (test using 'vars.Contains')
        for var, term in subst do
            if vars.Contains var then
                printf "%s = %s " var (formatTerm term)

        printfn ""


// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

let family =
    [ fact (Predicate("male", [ Atom("William") ]))
      fact (Predicate("female", [ Atom("Diana") ]))
      fact (Predicate("male", [ Atom("Charles") ]))
      fact (Predicate("male", [ Atom("George") ]))
      fact (Predicate("parent", [ Atom("Diana"); Atom("William") ]))
      fact (Predicate("parent", [ Atom("Charles"); Atom("William") ]))
      fact (Predicate("parent", [ Atom("William"); Atom("George") ]))
      rule
          (Predicate("father", [ Variable("X"); Variable("Y") ]))
          [ Predicate("parent", [ Variable("X"); Variable("Y") ])
            Predicate("male", [ Variable("X") ]) ] ]

// Queries from previous step (now called using 'run')
run family (Predicate("father", [ Variable("X"); Atom("William") ]))
run family (Predicate("father", [ Variable("X"); Variable("Y") ]))


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n =
    // Write a helper that generates a term representing number.
    // This should return Atom("zero") when n is 0 and otherwise
    // succ(succ(...(zero))) with appropriate number of 'succ's.
    if n = 0 then
        Atom("zero")
    else
        Predicate("succ", [ num (n - 1) ])



// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums =
    [ fact (Predicate("add", [ Atom("zero"); Variable("X"); Variable("X") ]))
      rule
          (Predicate(
              "add",
              [ Predicate("succ", [ Variable("X") ])
                Variable("Y")
                Predicate("succ", [ Variable("Z") ]) ]
          ))
          [ Predicate("add", [ Variable("X"); Variable("Y"); Variable("Z") ]) ]
      fact (Predicate("eq", [ Variable("X"); Variable("X") ])) ]

// Queries from previous step (now called using 'run')
run nums (Predicate("add", [ num 2; num 3; Variable("X") ]))
run nums (Predicate("add", [ num 2; Variable("X"); num 5 ]))
run nums (Predicate("add", [ num 2; Variable("Y"); Variable("X") ]))


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

// Helper that generates a term representing a list
let rec makeList l : Term =
    // Write a helper that generates a term representing a list.
    // This should return Atom("empty") when 'l' is [] and otherwise
    // cons(t1, .. cons(tN, empty)) when 'l' is [t1; ...; tN]
    match l with
    | [] -> Atom("empty")
    | h :: t -> Predicate("cons", [ h; (makeList t) ])


// TinyProlog code to represent 'append' operation on lists
// $ append([X|Y],Z,[X|W]) :- append(Y,Z,W).
// $ append([],X,X).
let append =
    [ fact (Predicate("append", [ Atom("empty"); Variable("X"); Variable("X") ]))
      rule
          (Predicate(
              "append",
              [ Predicate("cons", [ Variable("X"); Variable("Y") ])
                Variable("Z")
                Predicate("cons", [ Variable("X"); Variable("W") ]) ]
          ))
          [ Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ]) ] ]

let l1to4 = makeList [ for i in 1..4 -> num i ]
let l5to9 = makeList [ for i in 5..9 -> num i ]
let l1to9 = makeList [ for i in 1..9 -> num i ]

// Test the term formatting - this should print nice outputs!
formatTerm l1to4
formatTerm l5to9
formatTerm l1to9

// Query: append([1..4], [5..9], X)
// Return: X -> [1..9]
run append (Predicate("append", [ l1to4; l5to9; Variable "X" ]))

// Query: append([1..4], X, [1..9])
// Return: X -> [5..9]
run append (Predicate("append", [ l1to4; Variable "X"; l1to9 ]))

// Query: append(X, Y, [1..9])
// Return:
//  * X -> [1..9], Y -> []
//  * X -> [1..8], Y -> [9]
//  * X -> [1..7], Y -> [8, 9]
//  * X -> [1..6], Y -> [7 .. 9]
//  * etc.
run append (Predicate("append", [ Variable "Y"; Variable "X"; l1to9 ]))
