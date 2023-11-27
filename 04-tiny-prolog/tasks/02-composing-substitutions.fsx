// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
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
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
    (Predicate("loves", [ Atom("narcissus"); Atom("narcissus") ]))
    (Predicate("loves", [ Variable("X"); Variable("X") ]))

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
    (Predicate("loves", [ Atom("odysseus"); Atom("penelope") ]))
    (Predicate("loves", [ Variable("X"); Variable("X") ]))

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
    (Predicate("add", [ Atom("zero"); Predicate("succ", [ Atom("zero") ]) ]))
    (Predicate("add", [ Variable("Y"); Predicate("succ", [ Variable("Y") ]) ]))

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify (Predicate("loves", [ Variable("X"); Atom("narcissus") ])) (Predicate("loves", [ Variable("Y"); Variable("X") ]))

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
    (Predicate("add", [ Predicate("succ", [ Variable("X") ]); Variable("X") ]))
    (Predicate("add", [ Variable("Y"); Predicate("succ", [ Variable("Z") ]) ]))

