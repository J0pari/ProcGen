namespace TestInfrastructure

module TemporalLogic =

    open System
    open global.Xunit
    open System.Numerics

    /// Linear Temporal Logic formula types
    type LTL<'T> =
        | Atom of ('T -> bool)
        | Not of LTL<'T>
        | And of LTL<'T> * LTL<'T>
        | Or of LTL<'T> * LTL<'T>
        | Implies of LTL<'T> * LTL<'T>
        | Next of LTL<'T>
        | Eventually of LTL<'T>
        | Always of LTL<'T>
        | Until of LTL<'T> * LTL<'T>
        | Release of LTL<'T> * LTL<'T>
        | WeakUntil of LTL<'T> * LTL<'T>

    /// Execution trace
    type Trace<'T> = 'T list

    /// Check if LTL formula holds on trace
    let rec checkLTL (formula: LTL<'T>) (trace: Trace<'T>) : bool =
        match formula, trace with
        | Atom p, s :: _ -> p s
        | Atom _, [] -> false

        | Not phi, _ -> not (checkLTL phi trace)

        | And (phi, psi), _ -> checkLTL phi trace && checkLTL psi trace

        | Or (phi, psi), _ -> checkLTL phi trace || checkLTL psi trace

        | Implies (phi, psi), _ -> not (checkLTL phi trace) || checkLTL psi trace

        | Next phi, _ :: rest -> checkLTL phi rest
        | Next _, [] -> false

        | Eventually phi, _ ->
            let rec check remaining =
                match remaining with
                | [] -> false
                | _ when checkLTL phi remaining -> true
                | _ :: rest -> check rest
            check trace

        | Always phi, _ ->
            let rec check remaining =
                match remaining with
                | [] -> true
                | _ when not (checkLTL phi remaining) -> false
                | _ :: rest -> check rest
            check trace

        | Until (phi, psi), _ ->
            let rec check remaining =
                match remaining with
                | [] -> false
                | _ when checkLTL psi remaining -> true
                | _ when not (checkLTL phi remaining) -> false
                | _ :: rest -> check rest
            check trace

        | Release (phi, psi), _ ->
            let rec check remaining =
                match remaining with
                | [] -> true
                | _ when checkLTL phi remaining -> true
                | _ when not (checkLTL psi remaining) -> false
                | _ :: rest -> check rest
            check trace

        | WeakUntil (phi, psi), _ ->
            let rec check remaining =
                match remaining with
                | [] -> true
                | _ when checkLTL psi remaining -> true
                | _ when not (checkLTL phi remaining) -> false
                | _ :: rest -> check rest
            check trace

    /// Computation Tree Logic formula types
    type CTL<'T> =
        | CTLAtom of ('T -> bool)
        | CTLNot of CTL<'T>
        | CTLAnd of CTL<'T> * CTL<'T>
        | CTLOr of CTL<'T> * CTL<'T>
        | EX of CTL<'T>
        | AX of CTL<'T>
        | EF of CTL<'T>
        | AF of CTL<'T>
        | EG of CTL<'T>
        | AG of CTL<'T>
        | EU of CTL<'T> * CTL<'T>
        | AU of CTL<'T> * CTL<'T>

    /// State transition system
    type TransitionSystem<'T when 'T: comparison> = {
        States: Set<'T>
        InitialStates: Set<'T>
        Transitions: Map<'T, Set<'T>>
    }

    /// Check if CTL formula holds in transition system at state
    let rec checkCTL (system: TransitionSystem<'T>) (formula: CTL<'T>) (state: 'T) : bool =
        match formula with
        | CTLAtom p -> p state

        | CTLNot phi -> not (checkCTL system phi state)

        | CTLAnd (phi, psi) -> checkCTL system phi state && checkCTL system psi state

        | CTLOr (phi, psi) -> checkCTL system phi state || checkCTL system psi state

        | EX phi ->
            match Map.tryFind state system.Transitions with
            | Some successors -> successors |> Set.exists (checkCTL system phi)
            | None -> false

        | AX phi ->
            match Map.tryFind state system.Transitions with
            | Some successors -> successors |> Set.forall (checkCTL system phi)
            | None -> true

        | EF phi ->
            let rec explore visited current =
                if checkCTL system phi current then true
                else if Set.contains current visited then false
                else
                    match Map.tryFind current system.Transitions with
                    | Some successors ->
                        let newVisited = Set.add current visited
                        successors |> Set.exists (explore newVisited)
                    | None -> false
            explore Set.empty state

        | AF phi ->

            let rec explore visited current =
                if checkCTL system phi current then true
                else
                    match Map.tryFind current system.Transitions with
                    | Some successors when not (Set.isEmpty successors) ->
                        if Set.contains current visited then false
                        else
                            let newVisited = Set.add current visited
                            successors |> Set.forall (explore newVisited)
                    | _ -> false
            explore Set.empty state

        | EG phi ->

            let rec explore visited current =
                if not (checkCTL system phi current) then false
                elif Set.contains current visited then true
                else
                    match Map.tryFind current system.Transitions with
                    | Some successors ->
                        let newVisited = Set.add current visited
                        successors |> Set.exists (explore newVisited)
                    | None -> false
            explore Set.empty state

        | AG phi ->

            let rec explore visited current =
                if not (checkCTL system phi current) then false
                else
                    match Map.tryFind current system.Transitions with
                    | Some successors when not (Set.isEmpty successors) ->
                        if Set.contains current visited then true
                        else
                            let newVisited = Set.add current visited
                            successors |> Set.forall (explore newVisited)
                    | _ -> true
            explore Set.empty state

        | EU (phi, psi) ->

            let rec explore visited current =
                if checkCTL system psi current then true
                elif not (checkCTL system phi current) then false
                elif Set.contains current visited then false
                else
                    match Map.tryFind current system.Transitions with
                    | Some successors ->
                        let newVisited = Set.add current visited
                        successors |> Set.exists (explore newVisited)
                    | None -> false
            explore Set.empty state

        | AU (phi, psi) ->

            let rec explore visited current =
                if checkCTL system psi current then true
                elif not (checkCTL system phi current) then false
                else
                    match Map.tryFind current system.Transitions with
                    | Some successors when not (Set.isEmpty successors) ->
                        if Set.contains current visited then false
                        else
                            let newVisited = Set.add current visited
                            successors |> Set.forall (explore newVisited)
                    | _ -> false
            explore Set.empty state



    /// Safety property: bad states never occur
    let safety (bad: 'T -> bool) : LTL<'T> =
        Always (Not (Atom bad))

    /// Liveness property: good state eventually occurs
    let liveness (good: 'T -> bool) : LTL<'T> =
        Eventually (Atom good)

    /// Response property: every request gets an answer
    let response (request: 'T -> bool) (answer: 'T -> bool) : LTL<'T> =
        Always (Implies (Atom request, Eventually (Atom answer)))

    /// Precedence property: answer preceded by request
    let precedence (request: 'T -> bool) (answer: 'T -> bool) : LTL<'T> =
        Not (Until (Not (Atom request), Atom answer))

    /// Temporal logic pattern
    let stability (condition: 'T -> bool) : LTL<'T> =
        Eventually (Always (Atom condition))

    /// Temporal logic pattern
    let recurrence (event: 'T -> bool) : LTL<'T> =
        Always (Eventually (Atom event))

    /// Temporal logic pattern
    let mutualExclusion (inCS1: 'T -> bool) (inCS2: 'T -> bool) : LTL<'T> =
        Always (Not (And (Atom inCS1, Atom inCS2)))

    /// Temporal logic pattern
    let deadlockFree (canProgress: 'T -> bool) : LTL<'T> =
        Always (Eventually (Atom canProgress))

    /// Temporal logic pattern
    let starvationFree (waiting: 'T -> bool) (proceed: 'T -> bool) : LTL<'T> =
        Always (Implies (Atom waiting, Eventually (Atom proceed)))



    /// Temporal logic pattern
    let matchPattern (pattern: 'T -> bool) (trace: Trace<'T>) : Trace<'T> list =
        trace
        |> List.filter pattern
        |> List.singleton

    /// Temporal logic pattern
    let findOccurrences (predicate: 'T -> bool) (trace: Trace<'T>) : int list =
        trace
        |> List.indexed
        |> List.filter (fun (_, state) -> predicate state)
        |> List.map fst

    /// Temporal logic pattern
    let isMonotonic (measure: 'T -> float) (trace: Trace<'T>) : bool =
        trace
        |> List.map measure
        |> List.pairwise
        |> List.forall (fun (a, b) -> b >= a)

    /// Temporal logic pattern
    let isEventuallyMonotonic (measure: 'T -> float) (trace: Trace<'T>) : bool =
        let values = trace |> List.map measure
        let rec findMonotonicSuffix lst =
            match lst with
            | [] | [_] -> true
            | _ ->
                let isMonotonic = List.pairwise lst |> List.forall (fun (a, b) -> b >= a)
                if isMonotonic then true
                else findMonotonicSuffix (List.tail lst)
        findMonotonicSuffix values

    /// Temporal logic pattern
    let convergesTo (measure: 'T -> float) (target: float) (tolerance: float) (trace: Trace<'T>) : bool =
        let lastN = min 10 (List.length trace)
        trace
        |> List.rev
        |> List.take lastN
        |> List.map measure
        |> List.forall (fun v -> abs (v - target) < tolerance)

    /// Temporal logic pattern
    let detectOscillation (measure: 'T -> float) (trace: Trace<'T>) (threshold: float) : bool =
        trace
        |> List.map measure
        |> List.pairwise
        |> List.pairwise
        |> List.exists (fun ((a, b), (c, d)) ->
            let change1 = b - a
            let change2 = d - c
            abs change1 > threshold && abs change2 > threshold && sign change1 <> sign change2)

    /// Temporal logic pattern
    let distanceFromTarget (measure: 'T -> float) (target: float) (trace: Trace<'T>) : float list =
        trace |> List.map (fun state -> abs (measure state - target))



    /// Temporal logic pattern
    let isStrictlyIncreasing (measure: 'T -> float) (trace: Trace<'T>) : bool =
        trace
        |> List.map measure
        |> List.pairwise
        |> List.forall (fun (a, b) -> b > a)

    /// Temporal logic pattern
    let isBounded (measure: 'T -> float) (lower: float) (upper: float) (trace: Trace<'T>) : bool =
        trace
        |> List.map measure
        |> List.forall (fun v -> v >= lower && v <= upper)

    /// Temporal logic pattern
    let stabilizes (measure: 'T -> float) (tolerance: float) (windowSize: int) (trace: Trace<'T>) : bool =
        let values = trace |> List.map measure
        if List.length values < windowSize then false
        else
            values
            |> List.windowed windowSize
            |> List.exists (fun window ->
                let mean = List.average window
                window |> List.forall (fun v -> abs (v - mean) < tolerance))

    /// Temporal logic pattern
    let isPeriodic (measure: 'T -> float) (period: int) (tolerance: float) (trace: Trace<'T>) : bool =
        let values = trace |> List.map measure
        if List.length values < period * 3 then false
        else
            [0..period-1]
            |> List.forall (fun offset ->
                values
                |> List.indexed
                |> List.filter (fun (i, _) -> i % period = offset)
                |> List.map snd
                |> List.pairwise
                |> List.forall (fun (a, b) -> abs (b - a) < tolerance))






    /// Temporal logic pattern
    let generateTrace (initial: 'T) (step: 'T -> 'T) (length: int) : Trace<'T> =
        let rec generate state remaining =
            if remaining = 0 then []
            else state :: generate (step state) (remaining - 1)
        generate initial length

    /// Temporal logic pattern
    let generateTraceUntil (initial: 'T) (step: 'T -> 'T) (condition: 'T -> bool) (maxSteps: int) : Trace<'T> =
        let rec generate state remaining =
            if remaining = 0 || condition state then [state]
            else state :: generate (step state) (remaining - 1)
        generate initial maxSteps

    /// Temporal logic pattern
    let sampleTrace (interval: int) (trace: Trace<'T>) : Trace<'T> =
        trace
        |> List.indexed
        |> List.filter (fun (i, _) -> i % interval = 0)
        |> List.map snd
