module Flight.GeodesySolution

open Flight.Units.Angle
open Flight.LatLng
open Flight.Zone
open Flight.Earth.Ellipsoid
open Flight.Geodesy.Math
open Flight.Geodesy.Problem
module H = Earth.Sphere
module H = Haversine
module V = Vincenty

type IGeodesySolutions =
    abstract member AzimuthFwd : EarthMath * EarthModel -> AzimuthFwd
    abstract member AzimuthRev : EarthMath * EarthModel -> AzimuthRev
    abstract member ArcLength : EarthMath * EarthModel -> SpanLatLng

    abstract member Inverse
        : EarthMath * EarthModel
        -> InverseProblem<LatLng>
        -> GeodeticInverse<InverseSolution<TaskDistance, float<rad>>>

    abstract member Direct
        : EarthMath * EarthModel
        -> DirectProblem<LatLng, TrueCourse, Radius>
        -> GeodeticDirect<DirectSolution<LatLng, TrueCourse>>

let azimuthFwd : EarthMath * EarthModel -> AzimuthFwd =
    function
    | (Haversines, EarthAsSphere _) -> H.azimuthFwd
    | (Vincenty, EarthAsEllipsoid e) -> V.azimuthFwd e
    | _ -> failwith "Azimuth forward, unexpected combination of Earth math and model."

let azimuthRev : EarthMath * EarthModel -> AzimuthRev =
    function
    | (Haversines, EarthAsSphere _) -> H.azimuthRev
    | (Vincenty, EarthAsEllipsoid e) -> V.azimuthRev e
    | _ -> failwith "Azimuth reverse, unexpected combination of Earth math and model."

let arcLength : EarthMath * EarthModel -> SpanLatLng =
    function
    | (Haversines, EarthAsSphere _) -> H.distance
    | (Vincenty, EarthAsEllipsoid e) -> V.distance e
    | _ -> failwith "Arc length, unexpected combination of Earth math and model."

let inverse : EarthMath * EarthModel -> InverseProblem<LatLng> -> GeodeticInverse<InverseSolution<TaskDistance,float<rad>>> = 
    function
    | (Haversines, EarthAsSphere _) -> H.inverse >> GeodeticInverse.GeodeticInverse
    | (Vincenty, EarthAsEllipsoid e) -> V.inverse e defaultGeodeticAccuracy
    | _ -> failwith "Inverse solution, unexpected combination of Earth math and model."

let direct : EarthMath * EarthModel -> DirectProblem<LatLng, TrueCourse, Radius> -> GeodeticDirect<DirectSolution<LatLng, TrueCourse>> = 
    function
    | (Haversines, EarthAsSphere _) -> failwith "Not Implemented"
    | (Vincenty, EarthAsEllipsoid e) -> failwith "Not Implemented"
    | _ -> failwith "Direct solution, unexpected combination of Earth math and model."

type GeodesySolutions () =
    interface IGeodesySolutions with
        member x.AzimuthFwd (math, model) = azimuthFwd (math, model)
        member x.AzimuthRev (math, model) = azimuthRev (math, model)
        member x.ArcLength (math, model) = arcLength (math, model)
        member x.Direct (math, model) prob = direct (math, model) prob
        member x.Inverse (math, model) prob = inverse (math, model) prob