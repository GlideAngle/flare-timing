module rec Flight.Geodesy

open Flight.Units.Angle
open Flight.LatLng
open Flight.Zone
open Flight.Earth.Ellipsoid
open Flight.Geodesy.Math
open Flight.Geodesy.Problem
open Flight.Geodesy.Solution
module H = Flight.Earth.Sphere.Internal

module Math =
    open Flight.Zone

    type EarthMath = Haversines
    type EarthModel = EarthAsSphere of Radius


module Problem =
    /// <summary>The inputs for the direct or forward problem in geodesy.</summary>
    /// <param name="x">The departure point on the ellipsoid.</param>
    /// <param name="α₁">The azimuth from the departure point.</param>
    /// <param name="s">The distance to the arrival point.</param>
    type DirectProblem<'a, 'α, 's>  = {x : 'a; ``α₁`` : 'α; s : 's}

    /// <summary> The inputs for the inverse or reverse problem in geodesy.</summary>
    /// <param name="x">The departure point.</param>
    /// <param name="y">The arrival point.</param>
    type InverseProblem<'a> = {x : 'a; y : 'a}

    /// <summary>The outputs for the solution to the direct or forward problem in geodesy.</summary>
    /// <param name="y">The arrival point.</param>
    /// <param name="α₂">The azimuth at the arrival point.</param>
    type DirectSolution<'a, 'α> = {y : 'a; ``α₂`` : 'α option}

    /// <summary>The outputs for the solution to the inverse or reverse problem in geodesy.</summary>
    /// <param name="s">The distance between the departure and arrival points.</param>
    /// <param name="α₁">The azimuth at the departure point.</param>
    /// <param name="α₂">The azimuth at the arrival point.</param>
    type InverseSolution<'s, 'α> = {s : 's; ``α₁`` : 'α; ``α₂`` : 'α option}

    type IGeodesyProblems =
        abstract member Direct<'a, 'α, 's>
            : InverseProblem<'a>
            -> InverseSolution<'s, 'α>
            -> Option<DirectProblem<'a, 'α, 's> * DirectSolution<'a, 'α>>

        abstract member Inverse<'a, 'α, 's>
            : DirectProblem<'a, 'α, 's>
            -> DirectSolution<'a, 'α>
            -> Option<InverseProblem<'a> * InverseSolution<'s, 'α>>

module Solution =
    type IGeodesySolutions =
        abstract member AzimuthFwd : EarthMath * EarthModel -> AzimuthFwd
        abstract member AzimuthRev : EarthMath * EarthModel -> AzimuthRev
        abstract member ArcLength : EarthMath * EarthModel -> SpanLatLng

        abstract member Inverse
            : InverseProblem<LatLng>
            -> GeodeticInverse<InverseSolution<TaskDistance, float<rad>>>

        abstract member Direct
            : DirectProblem<LatLng, TrueCourse, Radius>
            -> GeodeticDirect<DirectSolution<LatLng, TrueCourse>>

let azimuthFwd : EarthMath * EarthModel -> AzimuthFwd = function
    | (Haversines, EarthAsSphere r) -> H.azimuthFwd

let azimuthRev : EarthMath * EarthModel -> AzimuthRev = function
    | (Haversines, EarthAsSphere r) -> H.azimuthRev

let arcLength : EarthMath * EarthModel -> SpanLatLng = function
    | (Haversines, EarthAsSphere r) -> H.distance

type GeodesySolutions () =
    interface IGeodesySolutions with
        member x.AzimuthFwd (math : EarthMath, model : EarthModel) : AzimuthRev = 
            azimuthFwd (math, model)

        member x.AzimuthRev (math : EarthMath, model : EarthModel) : AzimuthRev = 
            azimuthRev (math, model)

        member x.ArcLength (math : EarthMath, model : EarthModel) : SpanLatLng = 
            arcLength (math, model)

        member x.Direct(_ : DirectProblem<LatLng,TrueCourse,Radius>): GeodeticDirect<DirectSolution<LatLng,TrueCourse>> = 
            failwith "Not Implemented"

        member x.Inverse(_ : InverseProblem<LatLng>): GeodeticInverse<InverseSolution<TaskDistance,float<rad>>> = 
            failwith "Not Implemented"

