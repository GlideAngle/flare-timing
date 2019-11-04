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
[<InlineData(0.0, 0.0, 0.0, 0.0)>]
[<InlineData(90.0, 0.0, -90.0, 0.0)>]
[<InlineData(-90.0, 0.0, 90.0, 0.0)>]
[<InlineData(0.0, 180.0, 0.0, 0.0)>]
[<InlineData(0.0, -180.0, 0.0, 0.0)>]
[<InlineData(0.0, 0.0, 0.0, 180.0)>]
[<InlineData(0.0, 0.0, 0.0, -180.0)>]
[<InlineData(0.0, 180.0, 0.0, 180.0)>]
[<InlineData(0.0, 180.0, 0.0, -180.0)>]
[<InlineData(0.0, -180.0, 0.0, 180.0)>]
[<InlineData(0.0, -180.0, 0.0, -180.0)>]
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
    static let xys =
            [ ((( 55, 45,  0.0), (  0,  0,  0.0)), ((-33, 26,  0.0), (108, 13,  0.0)))
            ; ((( 37, 19, 54.95367), (  0,  0,  0.0)), (( 26,  7, 42.83946), ( 41, 28, 35.50729)))
            ; ((( 35, 16, 11.24862), (  0,  0,  0.0)), (( 67, 22, 14.77638), (137, 47, 28.31435)))
            ; (((  1,  0,  0.0), (  0,  0,  0.0)), (( 0, -59, 53.83076), (179, 17, 48.02997)))
            ; (((  1,  0,  0.0), (  0,  0,  0.0)), ((  1,  1, 15.18952), (179, 46, 17.84244)))
            ]
            |> List.map (fun ((xLat, xLng), (yLat, yLng)) ->
                (
                    { Lat = DMS.FromTuple xLat |> DMS.ToRad
                    ; Lng = DMS.FromTuple xLng |> DMS.ToRad
                    }
                ,
                    { Lat = DMS.FromTuple yLat |> DMS.ToRad
                    ; Lng = DMS.FromTuple yLng |> DMS.ToRad
                    }
                ))

    static let es = bessel :: List.replicate 4 hayford
    static let ds : float<m> list =
            [ 14110526.170<m>
            ; 4085966.703<m>
            ; 8084823.839<m>
            ; 19960000.000<m>
            ; 19780006.5584<m>
            ]

    static let fwdAzimuths : float<rad> list =
            [ ( 96, 36,  8.79960)
            ; ( 95, 27, 59.63089)
            ; ( 15, 44, 23.74850)
            ; ( 89,  0,  0.0)
            ; (  4, 59, 59.99995)
            ]
            |> List.map (DMS.FromTuple >> DMS.ToRad)

    static let revAzimuths : float<rad> list =
            [ (137, 52, 22.01454)
            ; (118,  5, 58.96161)
            ; (144, 55, 39.92147)
            ; ( 91,  0,  6.11733)
            ; (174, 59, 59.88481)
            ]
            |> List.map (DMS.FromTuple >> DMS.ToRad)

    static member DistanceData : seq<obj[]>=
        List.map3 (fun e (x, y) d -> (e, x, y, d, 0.001<m>)) es xys ds
        |> Seq.map FSharpValue.GetTupleFields

    static member AzimuthFwdData : seq<obj[]>=
        List.map3 (fun e (x, y) az -> (e, x, y, az, DMS (0<deg>, 0<min>, 0.016667<s>) |> DMS.ToRad)) es xys fwdAzimuths
        |> Seq.map FSharpValue.GetTupleFields

    static member AzimuthRevData : seq<obj[]>=
        List.map3 (fun e (x, y) az -> (e, x, y, az, DMS (0<deg>, 0<min>, 0.016667<s>) |> DMS.ToRad)) es xys revAzimuths
        |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("DistanceData", MemberType = typeof<Vincenty1975>)>]
let ``distances from Vincenty's 1975 paper`` (e, x, y, d, t) =
    test <@ let (TaskDistance d') = distance e x y in abs (d' - d) < t @>

[<Theory; MemberData("AzimuthFwdData", MemberType = typeof<Vincenty1975>)>]
let ``forward azimuth from Vincenty's 1975 paper`` (e, x, y, az, t) =
    test <@ azimuthFwd e x y |> function | None -> true | Some az' -> abs (az' - az) < t @>

[<Theory; MemberData("AzimuthRevData", MemberType = typeof<Vincenty1975>)>]
let ``reverse azimuth from Vincenty's 1975 paper`` (e, x, y, az, t) =
    test <@ azimuthRev e x y |> function | None -> true | Some az' -> abs (az' - az) < t @>
