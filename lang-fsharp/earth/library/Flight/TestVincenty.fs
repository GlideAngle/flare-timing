module TestVincenty

open System
open FSharp.Reflection
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Xunit
open Swensen.Unquote
open Hedgehog

open Flight.Units
open Flight.Units.Angle
open Flight.Units.Convert
open Flight.Zone
open Flight.LatLng
open Flight.Units.DegMinSec
open Flight.Earth.Ellipsoid
open Flight.Vincenty

[<Theory>]
[<InlineData(0.0, 0.0, 0.0, 0.0)>]
[<InlineData(Double.MinValue, 0.0, 0.0, 0.0)>]
[<InlineData(0.0, Double.MaxValue, 0.0, 0.0)>]
[<InlineData(0.0, 0.0, Double.MinValue, 0.0)>]
[<InlineData(0.0, 0.0, 0.0, Double.MaxValue)>]
let ``distance is not negative given latlng pairs in radians`` xLat xLng yLat yLng =
    let xLL = mkLatLng (xLat * 1.0<rad>) (xLng * 1.0<rad>)
    let yLL = mkLatLng (yLat * 1.0<rad>) (yLng * 1.0<rad>)
    match (xLL, yLL) with
    | (None, _) | (_, None) ->
        test <@ true @>

    | (Some xLL', Some yLL') ->
        test <@ distance wgs84 xLL' yLL' >= TaskDistance (0.0<m>) @>

[<Theory>]
[<InlineData(  0.0,    0.0,   0.0,    0.0)>]
[<InlineData( 90.0,    0.0, -90.0,    0.0)>]
[<InlineData(-90.0,    0.0,  90.0,    0.0)>]
[<InlineData(  0.0,  180.0,   0.0,    0.0)>]
[<InlineData(  0.0, -180.0,   0.0,    0.0)>]
[<InlineData(  0.0,    0.0,   0.0,  180.0)>]
[<InlineData(  0.0,    0.0,   0.0, -180.0)>]
[<InlineData(  0.0,  180.0,   0.0,  180.0)>]
[<InlineData(  0.0,  180.0,   0.0, -180.0)>]
[<InlineData(  0.0, -180.0,   0.0,  180.0)>]
[<InlineData(  0.0, -180.0,   0.0, -180.0)>]
let ``distance is not negative given latlng pairs in degrees`` xLat xLng yLat yLng =
    let xLL = mkLatLng (convertDegToRad <| xLat * 1.0<deg>) (convertDegToRad <| xLng * 1.0<deg>)
    let yLL = mkLatLng (convertDegToRad <| yLat * 1.0<deg>) (convertDegToRad <| yLng * 1.0<deg>)
    match (xLL, yLL) with
    | (None, _) | (_, None) ->
        test <@ true @>

    | (Some xLL', Some yLL') ->
        test <@ distance wgs84 xLL' yLL' >= TaskDistance (0.0<m>) @>

[<Fact>]
let ``distance is not negative forall latlng pairs`` () = Property.check <| property {
        let! xLat = Gen.double <| Range.constantBounded ()
        let! xLng = Gen.double <| Range.constantBounded ()
        let! yLat = Gen.double <| Range.constantBounded ()
        let! yLng = Gen.double <| Range.constantBounded ()
        let xLL = mkLatLng (xLat * 1.0<rad>) (xLng * 1.0<rad>)
        let yLL = mkLatLng (yLat * 1.0<rad>) (yLng * 1.0<rad>)
        match (xLL, yLL) with
        | (None, _) | (_, None) ->
            return true

        | (Some xLL', Some yLL') ->
            return distance wgs84 xLL' yLL' >= TaskDistance (0.0<m>)
    }

// SEE: https://stackoverflow.com/a/50905110/1503186
type Vincenty1975 () =
    // From the table in Vincenty's paper showing the results of solutions.
    static let xys : ((DMS * DMS) * (DMS * DMS)) list =
            [ ((( 55, 45,  0.00000), (  0,  0,  0.0)), ((-33, 26,  0.00000), (108, 13,  0.00000)))
            ; ((( 37, 19, 54.95367), (  0,  0,  0.0)), (( 26,  7, 42.83946), ( 41, 28, 35.50729)))
            ; ((( 35, 16, 11.24862), (  0,  0,  0.0)), (( 67, 22, 14.77638), (137, 47, 28.31435)))
            ; (((  1,  0,  0.00000), (  0,  0,  0.0)), (( 0, -59, 53.83076), (179, 17, 48.02997)))
            ; (((  1,  0,  0.00000), (  0,  0,  0.0)), ((  1,  1, 15.18952), (179, 46, 17.84244)))
            ]
            |> List.map (fun ((xLat, xLng), (yLat, yLng)) ->
                let x = (DMS.FromTuple xLat, DMS.FromTuple xLng)
                let y = (DMS.FromTuple yLat, DMS.FromTuple yLng)

                (x, y))

    static let es = bessel :: List.replicate 4 hayford

    static let ds : float<m> list =
            [ 14110526.170<m>
            ;  4085966.703<m>
            ;  8084823.839<m>
            ; 19960000.000<m>
            ; 19780006.5584<m>
            ]

    static let fwdAzimuths : DMS list =
            [ ( 96, 36,  8.79960)
            ; ( 95, 27, 59.63089)
            ; ( 15, 44, 23.74850)
            ; ( 89,  0,  0.00000)
            ; (  4, 59, 59.99995)
            ]
            |> List.map DMS.FromTuple

    static let revAzimuths : DMS list =
            [ (137, 52, 22.01454)
            ; (118,  5, 58.96161)
            ; (144, 55, 39.92147)
            ; ( 91,  0,  6.11733)
            ; (174, 59, 59.88481)
            ]
            |> List.map DMS.FromTuple

    // From the paper, Vincenty's errors were mm of -0.4, -0.4, -0.7, -0.2 and -0.8.
    static let directDistanceErrors : float<m> list =
            [ 0.000419<m>
            ; 0.000388<m>
            ; 0.000708<m>
            ; 0.000203<m>
            ; 0.000385<m>
            ]

    // From the paper, Vincenty's errors were s of -1.2, +0.5, +3.0, -0.3 and -0.3.
    static let directAzimuthRevErrors : DMS list =
            List.replicate 5 (DMS.FromTuple (0, 0, 0.0001))

    static member IndirectDistanceData : seq<obj[]>=
        List.map3 (fun e (x, y) d -> (e, x, y, d, 0.001<m>)) es xys ds
        |> Seq.map FSharpValue.GetTupleFields

    static member IndirectAzimuthFwdData : seq<obj[]>=
        List.map3
            (fun e (x, y) az -> (e, x, y, az, DMS (0<deg>, 0<min>, 0.016667<s>) |> DMS.ToRad))
            es xys fwdAzimuths
        |> Seq.map FSharpValue.GetTupleFields

    static member DirectDistanceData : seq<obj[]>=
        let eds = List.zip es ds
        let zts = List.zip fwdAzimuths directDistanceErrors
        List.map3 (fun (e, d) (x, y) (azFwd, t) -> (e, x, d, azFwd, y, t)) eds xys zts
        |> Seq.map FSharpValue.GetTupleFields

    static member DirectAzimuthRevData : seq<obj[]>=
        let eds = List.zip es ds
        let xrs = List.zip (List.map fst xys) directAzimuthRevErrors
        let zzs = List.zip fwdAzimuths revAzimuths
        List.map3 (fun (e, d) (x, t) (azFwd, azRev) -> (e, x, d, azFwd, azRev, t)) eds xrs zzs
        |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("IndirectDistanceData", MemberType = typeof<Vincenty1975>)>]
let ``indirect solution distance from Vincenty's 1975 paper`` (e, x, y, d, t) =
    let x' = LatLng.FromDMS x
    let y' = LatLng.FromDMS y
    test <@ let (TaskDistance d') = distance e x' y' in abs (d' - d) < t @>

[<Theory; MemberData("IndirectAzimuthFwdData", MemberType = typeof<Vincenty1975>)>]
let ``indirect solution forward azimuth from Vincenty's 1975 paper`` (e, x, y, az, t) =
    let f e x y =
        let azRad = DMS.ToRad az
        let x' = LatLng.FromDMS x
        let y' = LatLng.FromDMS y

        azimuthFwd e x' y'
        |> Option.map (fun az' -> abs (az' - azRad))

    test <@ f e x y < Some t @>

[<Theory; MemberData("DirectDistanceData", MemberType = typeof<Vincenty1975>)>]
let ``direct solution distance from Vicenty's 1975 paper`` (e, x, d, az, y, t) =
    let f e x az y : float<m> option =
        let azRad = DMS.ToRad az
        let x' = LatLng.FromDMS x
        let y' = LatLng.FromDMS y

        (direct e defaultGeodeticAccuracy {x = x'; ``α₁`` = TrueCourse azRad; s = Radius d})
        |> (function
            | GeodeticDirect soln ->
                let (TaskDistance d') = distance e y' soln.y
                Some <| abs d'
            | _ -> None)

    test <@ f e x az y < Some t @>

[<Theory; MemberData("DirectAzimuthRevData", MemberType = typeof<Vincenty1975>)>]
let ``direct solution reverse azimuth from Vincenty's 1975 paper`` (e, x, d, azFwd, azRev, t) =
    let f e x azFwd azRev =
        let azFwdRad = DMS.ToRad azFwd
        let azRevRad = DMS.ToRad azRev
        let x' = LatLng.FromDMS x

        (direct e defaultGeodeticAccuracy {x = x'; ``α₁`` = TrueCourse azFwdRad; s = Radius d})
        |> (function
            | GeodeticDirect soln ->
                soln.``α₂``
                |> Option.map (fun (TrueCourse az') -> abs (az' - azRevRad))
            | _ -> None)

    let t' = Some << DMS.ToRad <| t

    test <@ f e x azFwd azRev < t' @>
