namespace TestInfrastructure

module AlgebraicLaws =

    open System
    open FsCheck
    open FsCheck.FSharp
    open global.Xunit

    let associativity (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) : unit =
        let prop (a: 'T) (b: 'T) (c: 'T) = op (op a b) c = op a (op b c)
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> Prop.forAll arb (fun c -> prop a b c))))

    let commutativity (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) : unit =
        let prop (a: 'T) (b: 'T) = op a b = op b a
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> prop a b)))

    let leftIdentity (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) (identity: 'T) : unit =
        let prop (a: 'T) = op identity a = a
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let rightIdentity (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) (identity: 'T) : unit =
        let prop (a: 'T) = op a identity = a
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let leftInverse (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) (inv: 'T -> 'T) (identity: 'T) : unit =
        let prop (a: 'T) = op (inv a) a = identity
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let rightInverse (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) (inv: 'T -> 'T) (identity: 'T) : unit =
        let prop (a: 'T) = op a (inv a) = identity
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let distributivity (arb: Arbitrary<'T>) (mul: 'T -> 'T -> 'T) (add: 'T -> 'T -> 'T) : unit =
        let prop (a: 'T) (b: 'T) (c: 'T) = mul a (add b c) = add (mul a b) (mul a c)
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> Prop.forAll arb (fun c -> prop a b c))))

    let absorptionLeft (arb: Arbitrary<'T>) (op1: 'T -> 'T -> 'T) (op2: 'T -> 'T -> 'T) : unit =
        let prop (a: 'T) (b: 'T) = op1 a (op2 a b) = a
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> prop a b)))

    let absorptionRight (arb: Arbitrary<'T>) (op1: 'T -> 'T -> 'T) (op2: 'T -> 'T -> 'T) : unit =
        let prop (a: 'T) (b: 'T) = op1 (op2 a b) b = b
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> prop a b)))

    let idempotence (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) : unit =
        let prop (a: 'T) = op a a = a
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let functorIdentity (arb: Arbitrary<'F>) (fmap: ('A -> 'A) -> 'F -> 'F) : unit =
        let prop (fa: 'F) = fmap id fa = fa
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let functorComposition (arb: Arbitrary<'F>) (fmap: ('B -> 'C) -> 'F -> 'F) (f: 'A -> 'B) (g: 'B -> 'C) : unit =
        let prop (fa: 'F) = fmap (g << f) fa = (fmap g (fmap f fa))
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let monadLeftIdentity (return': 'A -> 'M) (bind: ('A -> 'M) -> 'M -> 'M) (a: 'A) (f: 'A -> 'M) : unit =
        let left = bind f (return' a)
        let right = f a
        Assert.Equal<'M>(right, left)

    let monadRightIdentity (arb: Arbitrary<'M>) (return': 'A -> 'M) (bind: ('A -> 'M) -> 'M -> 'M) : unit =
        let prop (m: 'M) = bind return' m = m
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let monadAssociativity (arb: Arbitrary<'M>) (bind: ('B -> 'M) -> 'M -> 'M) (f: 'A -> 'M) (g: 'B -> 'M) : unit =
        let prop (m: 'M) = bind g (bind f m) = bind (fun x -> bind g (f x)) m
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let monoidAssociativity (arb: Arbitrary<'T>) (mappend: 'T -> 'T -> 'T) : unit =
        associativity arb mappend

    let monoidIdentity (arb: Arbitrary<'T>) (mappend: 'T -> 'T -> 'T) (mempty: 'T) : unit =
        leftIdentity arb mappend mempty
        rightIdentity arb mappend mempty

    let roundTrip (arbA: Arbitrary<'A>) (encode: 'A -> 'B) (decode: 'B -> 'A) : unit =
        let prop (a: 'A) = decode (encode a) = a
        Check.QuickThrowOnFailure (Prop.forAll arbA prop)

    let doubleRoundTrip (arbA: Arbitrary<'A>) (arbB: Arbitrary<'B>) (encode: 'A -> 'B) (decode: 'B -> 'A) : unit =
        roundTrip arbA encode decode
        let prop (b: 'B) = encode (decode b) = b
        Check.QuickThrowOnFailure (Prop.forAll arbB prop)

    let determinism (arb: Arbitrary<'T>) (f: 'T -> 'R) : unit =
        let prop (x: 'T) = f x = f x
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let idempotenceUnary (arb: Arbitrary<'T>) (f: 'T -> 'T) : unit =
        let prop (x: 'T) = f (f x) = f x
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let involution (arb: Arbitrary<'T>) (f: 'T -> 'T) : unit =
        let prop (x: 'T) = f (f x) = x
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let homomorphism (arbA: Arbitrary<'A>) (opA: 'A -> 'A -> 'A) (opB: 'B -> 'B -> 'B) (h: 'A -> 'B) : unit =
        let prop (a1: 'A) (a2: 'A) = h (opA a1 a2) = opB (h a1) (h a2)
        Check.QuickThrowOnFailure (Prop.forAll arbA (fun a1 -> Prop.forAll arbA (fun a2 -> prop a1 a2)))

    let antiSymmetry (arb: Arbitrary<'T>) (le: 'T -> 'T -> bool) : unit =
        let prop (a: 'T) (b: 'T) = not (le a b && le b a) || a = b
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> prop a b)))

    let reflexivity (arb: Arbitrary<'T>) (rel: 'T -> 'T -> bool) : unit =
        let prop (a: 'T) = rel a a
        Check.QuickThrowOnFailure (Prop.forAll arb prop)

    let transitivity (arb: Arbitrary<'T>) (rel: 'T -> 'T -> bool) : unit =
        let prop (a: 'T) (b: 'T) (c: 'T) = not (rel a b && rel b c) || rel a c
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> Prop.forAll arb (fun c -> prop a b c))))

    let totality (arb: Arbitrary<'T>) (le: 'T -> 'T -> bool) : unit =
        let prop (a: 'T) (b: 'T) = le a b || le b a
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> prop a b)))

    type Semigroup<'T> = {
        Append: 'T -> 'T -> 'T
    }

    type Monoid<'T> = {
        Semigroup: Semigroup<'T>
        Empty: 'T
    }

    type Group<'T> = {
        Monoid: Monoid<'T>
        Inverse: 'T -> 'T
    }

    let checkSemigroup (arb: Arbitrary<'T>) (sg: Semigroup<'T>) : unit =
        associativity arb sg.Append

    let checkMonoid (arb: Arbitrary<'T>) (m: Monoid<'T>) : unit =
        checkSemigroup arb m.Semigroup
        leftIdentity arb m.Semigroup.Append m.Empty
        rightIdentity arb m.Semigroup.Append m.Empty

    let checkGroup (arb: Arbitrary<'T>) (g: Group<'T>) : unit =
        checkMonoid arb g.Monoid
        leftInverse arb g.Monoid.Semigroup.Append g.Inverse g.Monoid.Empty
        rightInverse arb g.Monoid.Semigroup.Append g.Inverse g.Monoid.Empty

    let checkPartialOrder (arb: Arbitrary<'T>) (le: 'T -> 'T -> bool) : unit =
        reflexivity arb le
        antiSymmetry arb le
        transitivity arb le

    let checkTotalOrder (arb: Arbitrary<'T>) (le: 'T -> 'T -> bool) : unit =
        checkPartialOrder arb le
        totality arb le

    let checkEquivalenceRelation (arb: Arbitrary<'T>) (equiv: 'T -> 'T -> bool) : unit =
        reflexivity arb equiv
        let symmetry (a: 'T) (b: 'T) = equiv a b = equiv b a
        Check.QuickThrowOnFailure (Prop.forAll arb (fun a -> Prop.forAll arb (fun b -> symmetry a b)))
        transitivity arb equiv

    let discoverLaws (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) : string list =
        let mutable laws = []

        try associativity arb op; laws <- "associative" :: laws with _ -> ()
        try commutativity arb op; laws <- "commutative" :: laws with _ -> ()
        try idempotence arb op; laws <- "idempotent" :: laws with _ -> ()

        laws

    let discoverIdentity (arb: Arbitrary<'T>) (op: 'T -> 'T -> 'T) (candidates: 'T list) : 'T option =
        candidates
        |> List.tryFind (fun candidate ->
            try
                leftIdentity arb op candidate
                rightIdentity arb op candidate
                true
            with _ -> false)
