// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
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
    let n = nextNumber()

    let variables =
        List.distinct (freeVariables clause.Head @ List.collect freeVariables clause.Body)

    let subst =
        Map.ofList (List.map (fun name -> (name, Variable(name + n.ToString()))) variables)

    { Head = (substitute subst clause.Head)
      Body = (substituteTerms subst clause.Body) }


let query (program: list<Clause>) (query: Term) : list<Clause * list<string * Term>> =
    // TODO: Return all clauses from 'program' whose 'Head' can be
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


// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule
    (Predicate("grandparent", [ Variable("X"); Variable("Y") ]))
    [ Predicate("parent", [ Variable("X"); Variable("Z") ])
      Predicate("parent", [ Variable("Z"); Variable("Y") ]) ]
|> withFreshVariables

// Some information about the British royal family
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

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [ Variable("X") ]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [ Variable("X"); Atom("William") ]))
