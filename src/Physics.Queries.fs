namespace Physics

open System
open System.Numerics

/// Spatial query API for raycasts, sphere casts, overlap tests
module Queries =

    /// Ray definition
    type Ray = {
        Origin: Vector3
        Direction: Vector3  // Should be normalized
        MaxDistance: float
    }

    /// Raycast hit result
    type RaycastHit = {
        BodyHandle: Service.BodyHandle
        Point: Vector3       // World space hit point
        Normal: Vector3      // Surface normal at hit
        Distance: float      // Distance from ray origin
        Fraction: float      // 0-1 along ray (distance / maxDistance)
    }

    /// Sphere shape for casting/overlap
    type Sphere = {
        Center: Vector3
        Radius: float
    }

    /// Box shape for overlap queries
    type Box = {
        Center: Vector3
        HalfExtents: Vector3
        Rotation: Quaternion
    }

    /// Capsule shape
    type Capsule = {
        Point1: Vector3
        Point2: Vector3
        Radius: float
    }

    /// Query filter for selective testing
    type QueryFilter = {
        LayerMask: int option         // Bit mask for layers
        ExcludeBodies: Service.BodyHandle list
        IncludeStatic: bool
        IncludeKinematic: bool
    }

    let defaultFilter = {
        LayerMask = None
        ExcludeBodies = []
        IncludeStatic = true
        IncludeKinematic = true
    }

    /// Ray-sphere intersection test
    let private raySphereIntersect (ray: Ray) (center: Vector3) (radius: float) : float option =
        let m = ray.Origin - center
        let b = Vector3.Dot(m, ray.Direction)
        let c = Vector3.Dot(m, m) - float32 (radius * radius)

        // Exit if ray origin outside sphere and pointing away
        if c > 0.0f && b > 0.0f then
            None
        else
            let discr = b * b - c
            if discr < 0.0f then
                None  // No intersection
            else
                let t = -b - sqrt discr
                let dist = max 0.0f t  // Clamp to ray origin
                if float dist <= ray.MaxDistance then
                    Some (float dist)
                else
                    None

    /// Sphere-sphere overlap test
    let private sphereSphereOverlap (c1: Vector3) (r1: float) (c2: Vector3) (r2: float) : bool =
        let delta = c2 - c1
        let distSq = delta.LengthSquared()
        let radiusSum = r1 + r2
        distSq <= float32 (radiusSum * radiusSum)

    /// AABB-AABB overlap test
    let private aabbOverlap (min1: Vector3) (max1: Vector3) (min2: Vector3) (max2: Vector3) : bool =
        min1.X <= max2.X && max1.X >= min2.X &&
        min1.Y <= max2.Y && max1.Y >= min2.Y &&
        min1.Z <= max2.Z && max1.Z >= min2.Z

    /// Point inside AABB test
    let private pointInAABB (point: Vector3) (min: Vector3) (max: Vector3) : bool =
        point.X >= min.X && point.X <= max.X &&
        point.Y >= min.Y && point.Y <= max.Y &&
        point.Z >= min.Z && point.Z <= max.Z

    /// Raycast against all bodies in world
    let raycast (world: Service.PhysicsWorld) (ray: Ray) (filter: QueryFilter) : RaycastHit option =
        let stats = world.GetStatistics()
        let mutable closestHit : RaycastHit option = None
        let mutable closestDist = ray.MaxDistance

        // Iterate all bodies (simplified - real impl would use BVH)
        for handle in 0 .. stats.BodyCount - 1 do
            match world.GetRigidBody(handle) with
            | Some body when not (List.contains handle filter.ExcludeBodies) ->
                // Filter static/kinematic
                if (filter.IncludeStatic || not body.IsStatic) &&
                   (filter.IncludeKinematic || not body.IsKinematic) then

                    // Simplified: treat all bodies as unit spheres
                    let sphereRadius = 1.0
                    match raySphereIntersect ray body.Position sphereRadius with
                    | Some dist when dist < closestDist ->
                        closestDist <- dist
                        let hitPoint = ray.Origin + ray.Direction * float32 dist
                        let normal = Vector3.Normalize(hitPoint - body.Position)
                        closestHit <- Some {
                            BodyHandle = handle
                            Point = hitPoint
                            Normal = normal
                            Distance = dist
                            Fraction = dist / ray.MaxDistance
                        }
                    | _ -> ()
            | _ -> ()

        closestHit

    /// Raycast returning all hits (not just closest)
    let raycastAll (world: Service.PhysicsWorld) (ray: Ray) (filter: QueryFilter) : RaycastHit array =
        let stats = world.GetStatistics()
        let hits = ResizeArray<RaycastHit>()

        for handle in 0 .. stats.BodyCount - 1 do
            match world.GetRigidBody(handle) with
            | Some body when not (List.contains handle filter.ExcludeBodies) ->
                if (filter.IncludeStatic || not body.IsStatic) &&
                   (filter.IncludeKinematic || not body.IsKinematic) then

                    let sphereRadius = 1.0
                    match raySphereIntersect ray body.Position sphereRadius with
                    | Some dist ->
                        let hitPoint = ray.Origin + ray.Direction * float32 dist
                        let normal = Vector3.Normalize(hitPoint - body.Position)
                        hits.Add({
                            BodyHandle = handle
                            Point = hitPoint
                            Normal = normal
                            Distance = dist
                            Fraction = dist / ray.MaxDistance
                        })
                    | _ -> ()
            | _ -> ()

        hits.ToArray()
        |> Array.sortBy (fun hit -> hit.Distance)

    /// Sphere cast (swept sphere)
    let sphereCast (world: Service.PhysicsWorld) (sphere: Sphere) (direction: Vector3) (maxDistance: float) (filter: QueryFilter) : RaycastHit option =
        let ray = {
            Origin = sphere.Center
            Direction = Vector3.Normalize(direction)
            MaxDistance = maxDistance
        }

        let stats = world.GetStatistics()
        let mutable closestHit : RaycastHit option = None
        let mutable closestDist = maxDistance

        for handle in 0 .. stats.BodyCount - 1 do
            match world.GetRigidBody(handle) with
            | Some body when not (List.contains handle filter.ExcludeBodies) ->
                if (filter.IncludeStatic || not body.IsStatic) &&
                   (filter.IncludeKinematic || not body.IsKinematic) then

                    let bodyRadius = 1.0
                    let expandedRadius = sphere.Radius + bodyRadius

                    match raySphereIntersect ray body.Position expandedRadius with
                    | Some dist when dist < closestDist ->
                        closestDist <- dist
                        let hitPoint = ray.Origin + ray.Direction * float32 dist
                        let normal = Vector3.Normalize(hitPoint - body.Position)
                        closestHit <- Some {
                            BodyHandle = handle
                            Point = hitPoint
                            Normal = normal
                            Distance = dist
                            Fraction = dist / maxDistance
                        }
                    | _ -> ()
            | _ -> ()

        closestHit

    /// Overlap sphere - find all bodies overlapping sphere
    let overlapSphere (world: Service.PhysicsWorld) (sphere: Sphere) (filter: QueryFilter) : Service.BodyHandle array =
        let stats = world.GetStatistics()
        let overlapping = ResizeArray<Service.BodyHandle>()

        for handle in 0 .. stats.BodyCount - 1 do
            match world.GetRigidBody(handle) with
            | Some body when not (List.contains handle filter.ExcludeBodies) ->
                if (filter.IncludeStatic || not body.IsStatic) &&
                   (filter.IncludeKinematic || not body.IsKinematic) then

                    let bodyRadius = 1.0
                    if sphereSphereOverlap sphere.Center sphere.Radius body.Position bodyRadius then
                        overlapping.Add(handle)
            | _ -> ()

        overlapping.ToArray()

    /// Overlap box - find all bodies overlapping oriented box
    let overlapBox (world: Service.PhysicsWorld) (box: Box) (filter: QueryFilter) : Service.BodyHandle array =
        let stats = world.GetStatistics()
        let overlapping = ResizeArray<Service.BodyHandle>()

        // Simplified: convert box to AABB (ignore rotation)
        let min = box.Center - box.HalfExtents
        let max = box.Center + box.HalfExtents

        for handle in 0 .. stats.BodyCount - 1 do
            match world.GetRigidBody(handle) with
            | Some body when not (List.contains handle filter.ExcludeBodies) ->
                if (filter.IncludeStatic || not body.IsStatic) &&
                   (filter.IncludeKinematic || not body.IsKinematic) then

                    // Simplified: treat body as point with radius
                    let bodyRadius = 1.0f
                    let bodyMin = body.Position - Vector3(bodyRadius)
                    let bodyMax = body.Position + Vector3(bodyRadius)

                    if aabbOverlap min max bodyMin bodyMax then
                        overlapping.Add(handle)
            | _ -> ()

        overlapping.ToArray()

    /// Overlap capsule - find all bodies overlapping capsule
    let overlapCapsule (world: Service.PhysicsWorld) (capsule: Capsule) (filter: QueryFilter) : Service.BodyHandle array =
        let stats = world.GetStatistics()
        let overlapping = ResizeArray<Service.BodyHandle>()

        let axis = capsule.Point2 - capsule.Point1
        let axisLen = axis.Length()
        let axisNorm = if axisLen > 0.001f then axis / axisLen else Vector3.UnitY

        for handle in 0 .. stats.BodyCount - 1 do
            match world.GetRigidBody(handle) with
            | Some body when not (List.contains handle filter.ExcludeBodies) ->
                if (filter.IncludeStatic || not body.IsStatic) &&
                   (filter.IncludeKinematic || not body.IsKinematic) then

                    // Point-to-line-segment distance
                    let ap = body.Position - capsule.Point1
                    let t = Vector3.Dot(ap, axisNorm)
                    let t_clamped = max 0.0f (min axisLen t)
                    let closestPoint = capsule.Point1 + axisNorm * t_clamped

                    let dist = Vector3.Distance(body.Position, closestPoint)
                    let bodyRadius = 1.0f

                    if float dist <= (capsule.Radius + float bodyRadius) then
                        overlapping.Add(handle)
            | _ -> ()

        overlapping.ToArray()

    /// Check if point is inside any collider
    let pointTest (world: Service.PhysicsWorld) (point: Vector3) (filter: QueryFilter) : Service.BodyHandle option =
        let stats = world.GetStatistics()

        [0 .. stats.BodyCount - 1]
        |> List.tryPick (fun handle ->
            match world.GetRigidBody(handle) with
            | Some body when not (List.contains handle filter.ExcludeBodies) ->
                if (filter.IncludeStatic || not body.IsStatic) &&
                   (filter.IncludeKinematic || not body.IsKinematic) then

                    let bodyRadius = 1.0f
                    let dist = Vector3.Distance(point, body.Position)

                    if dist <= bodyRadius then Some handle else None
                else None
            | _ -> None
        )

    /// Get closest point on all colliders to a world point
    let closestPoint (world: Service.PhysicsWorld) (point: Vector3) (maxDistance: float) (filter: QueryFilter) : (Service.BodyHandle * Vector3 * float) option =
        let stats = world.GetStatistics()
        let mutable closest : (Service.BodyHandle * Vector3 * float) option = None
        let mutable closestDist = maxDistance

        for handle in 0 .. stats.BodyCount - 1 do
            match world.GetRigidBody(handle) with
            | Some body when not (List.contains handle filter.ExcludeBodies) ->
                if (filter.IncludeStatic || not body.IsStatic) &&
                   (filter.IncludeKinematic || not body.IsKinematic) then

                    let bodyRadius = 1.0f
                    let toBody = body.Position - point
                    let dist = toBody.Length()

                    // Closest point on sphere surface
                    let surfaceDist = max 0.0f (dist - bodyRadius)

                    if float surfaceDist < closestDist then
                        closestDist <- float surfaceDist
                        let closestPt =
                            if dist > 0.001f then
                                point + toBody * (min dist bodyRadius) / dist
                            else
                                body.Position

                        closest <- Some (handle, closestPt, float surfaceDist)
            | _ -> ()

        closest
