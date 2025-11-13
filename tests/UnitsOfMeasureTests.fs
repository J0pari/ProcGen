namespace Tests

module UnitsOfMeasureTests =

    open System
    open global.Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open TestInfrastructure.Core
    open TestInfrastructure.Assertions
    open TestInfrastructure.PropertyDSL
    open TestInfrastructure.Builders

    [<Measure>] type m
    [<Measure>] type s
    [<Measure>] type kg
    [<Measure>] type N = kg m / s^2
    [<Measure>] type J = N m
    [<Measure>] type W = J / s

    type Spring<[<Measure>] 'u> = {
        RestLength: float<'u>
        Stiffness: float<N/'u>
        Damping: float<N s/'u>
    }

    type Vector3<[<Measure>] 'u> = { X: float<'u>; Y: float<'u>; Z: float<'u> }

    type PhysicsState<[<Measure>] 'u> = {
        Position: Vector3<'u>
        Velocity: Vector3<'u/s>
        TimeStep: float<s>
    }

    [<Fact>]
    let ``Distance divided by time yields velocity`` () =
        let distance = 100.0<m>
        let time = 10.0<s>
        let velocity: float<m/s> = distance / time
        shouldBeWithin 0.001 10.0 (float velocity)

    [<Fact>]
    let ``Force times distance yields energy`` () =
        let force = 50.0<N>
        let distance = 2.0<m>
        let energy: float<J> = force * distance
        shouldBeWithin 0.001 100.0 (float energy)

    [<Fact>]
    let ``Energy divided by time yields power`` () =
        let energy = 1000.0<J>
        let time = 5.0<s>
        let power: float<W> = energy / time
        shouldBeWithin 0.001 200.0 (float power)

    [<Fact>]
    let ``Spring force has correct units`` () =
        let spring = { RestLength = 10.0<m>; Stiffness = 50.0<N/m>; Damping = 0.5<N s/m> }
        let displacement = 2.0<m>
        let force: float<N> = spring.Stiffness * displacement
        shouldBeWithin 0.001 100.0 (float force)

    [<Fact>]
    let ``Kinetic energy from mass and velocity`` () =
        let mass = 2.0<kg>
        let velocity = 5.0<m/s>
        let kineticEnergy: float<J> = 0.5 * mass * velocity * velocity
        shouldBeWithin 0.001 25.0 (float kineticEnergy)

    [<Fact>]
    let ``Dimensionless ratio from same units`` () =
        let x = 20.0<m>
        let y = 5.0<m>
        let ratio: float = x / y
        shouldBeWithin 0.001 4.0 ratio

    [<Fact>]
    let ``Unit conversion roundtrip`` () =
        let original = 15.0<m>
        let stripped = float original
        let restored = stripped * 1.0<m>
        shouldBeWithin 0.001 (float original) (float restored)

    [<Fact>]
    let ``Spring with units builder`` () =
        let spring = {
            RestLength = 8.0<m>
            Stiffness = 100.0<N/m>
            Damping = 1.0<N s/m>
        }
        shouldBeWithin 0.001 8.0 (float spring.RestLength)
        shouldBeWithin 0.001 100.0 (float spring.Stiffness)

    [<Fact>]
    let ``PhysicsState preserves units through update`` () =
        let state = {
            Position = Vector3(10.0<m>, 20.0<m>, 30.0<m> )
            Velocity = Vector3(1.0<m/s>, 2.0<m/s>, 3.0<m/s> )
            TimeStep = 0.1<s>
        }
        let newPos = Vector3(state.Position.X + state.Velocity.X * state.TimeStep
            Y = state.Position.Y + state.Velocity.Y * state.TimeStep
            Z = state.Position.Z + state.Velocity.Z * state.TimeStep
        }
        shouldBeWithin 0.001 10.1 (float newPos.X)
        shouldBeWithin 0.001 20.2 (float newPos.Y)

    [<Fact>]
    let ``Verlet integration preserves units`` () =
        let dt = 0.016<s>
        let pos = { X = 0.0<m>, 0.0<m>, 0.0<m> )
        let vel = Vector3(10.0<m/s>, 0.0<m/s>, 0.0<m/s> )
        let acc = Vector3(0.0<m/s^2>, -9.8<m/s^2>, 0.0<m/s^2> )

        let newPos: Vector3<m> = {
            X = pos.X + vel.X * dt + acc.X * dt * dt * 0.5
            Y = pos.Y + vel.Y * dt + acc.Y * dt * dt * 0.5
            Z = pos.Z + vel.Z * dt + acc.Z * dt * dt * 0.5
        }
        let newVel: Vector3<m/s> = {
            X = vel.X + acc.X * dt
            Y = vel.Y + acc.Y * dt
            Z = vel.Z + acc.Z * dt
        }

        shouldBeWithin 0.001 0.16 (float newPos.X)
        shouldBeWithin 0.001 -0.00125 (float newPos.Y)

    [<Fact>]
    let ``Power from force and velocity`` () =
        let force = 100.0<N>
        let velocity = 5.0<m/s>
        let power: float<W> = force * velocity
        shouldBeWithin 0.001 500.0 (float power)

    [<Fact>]
    let ``Spring potential energy`` () =
        let k = 200.0<N/m>
        let x = 0.5<m>
        let energy: float<J> = 0.5 * k * x * x
        shouldBeWithin 0.001 25.0 (float energy)

    [<Fact>]
    let ``Velocity multiplication by scalar`` () =
        let v = 10.0<m/s>
        let scaled: float<m/s> = v * 3.0
        shouldBeWithin 0.001 30.0 (float scaled)

    [<Fact>]
    let ``Acceleration from velocity change`` () =
        let dv = 20.0<m/s>
        let dt = 4.0<s>
        let acc: float<m/s^2> = dv / dt
        shouldBeWithin 0.001 5.0 (float acc)

    [<Fact>]
    let ``Newton second law F equals ma`` () =
        let mass = 5.0<kg>
        let acceleration = 2.0<m/s^2>
        let force: float<N> = mass * acceleration
        shouldBeWithin 0.001 10.0 (float force)

    [<Fact>]
    let ``Damping force from velocity`` () =
        let damping = 0.8<N s/m>
        let velocity = 5.0<m/s>
        let dampingForce: float<N> = damping * velocity
        shouldBeWithin 0.001 4.0 (float dampingForce)

    [<Property>]
    let ``Units preserve equality`` (x: float) =
        x > 0.0 ==> lazy (
            let withUnit = x * 1.0<m>
            let stripped = float withUnit
            abs (stripped - x) < 1e-10
        )

    [<Property>]
    let ``Velocity units compose correctly`` (d: PositiveInt) (t: PositiveInt) =
        let (PositiveInt dVal) = d
        let (PositiveInt tVal) = t
        let distance = float dVal * 1.0<m>
        let time = float tVal * 1.0<s>
        let velocity: float<m/s> = distance / time
        float velocity = (float dVal / float tVal)

    [<Property>]
    let ``Energy units compose from force and distance`` (f: PositiveInt) (d: PositiveInt) =
        let (PositiveInt fVal) = f
        let (PositiveInt dVal) = d
        let force = float fVal * 1.0<N>
        let distance = float dVal * 1.0<m>
        let energy: float<J> = force * distance
        float energy = (float fVal * float dVal)

    [<Fact>]
    let ``Type annotations compile successfully`` () =
        let length: float<m> = 15.0<m>
        let time: float<s> = 3.0<s>
        let velocity: float<m/s> = length / time
        let mass: float<kg> = 10.0<kg>
        let force: float<N> = mass * (velocity / time)
        shouldBeWithin 0.001 50.0 (float force)

    [<Fact>]
    let ``No runtime overhead units erased`` () =
        let withUnits = 100.0<m>
        let withoutUnits = 100.0
        shouldBeWithin 1e-15 withoutUnits (float withUnits)
