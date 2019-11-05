module TestBedford

open FSharp.Reflection
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Xunit
open Swensen.Unquote

open Flight.Units
open Flight.Units.Angle
open Flight.Zone
open Flight.LatLng
open Flight.Units.DegMinSec
open Flight.Earth.Ellipsoid
open Flight.Geodesy.Problem
open Flight.Vincenty

type Bedford1978 () =
    // The pairs of pairs of angles being the input points for the inverse
    // problem.
    static let xys : ((DMS * DMS) * (DMS * DMS)) list =
            [ (((10,  0,  0.0), (-18,  0,  0.0)), ((10, 43, 39.078), (-18,  0,  0.000)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((40, 43, 28.790), (-18,  0,  0.000)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 43, 16.379), (-18,  0,  0.000)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), ((10, 30, 50.497), (-17, 28, 48.777)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((40, 30, 37.757), (-17, 19, 43.280)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 30, 12.925), (-16, 28, 22.844)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 59, 57.087), (-17, 15, 57.926)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 59, 46.211), (-17,  3, 27.942)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((69, 59, 15.149), (-15, 53, 37.449)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 38,  8.260), (-17, 21, 54.407)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 31, 54.913), (-18, 43,  1.027)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 42, 35.533), (-18, 22, 43.683)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), ((14, 21, 52.456), (-18,  0,  0.000)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((44, 20, 47.740), (-18,  0,  0.000)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((74, 19, 35.289), (-18,  0,  0.000)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), ((13,  4, 12.564), (-14, 51, 13.283)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((43,  0,  0.556), (-13, 48, 49.111)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((72, 47, 48.242), ( -7, 36, 58.487)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 58, 15.192), (-13, 35, 48.467)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 51, 44.295), (-12, 21, 14.090)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((69, 33, 22.562), ( -5, 32,  1.822)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), ((17, 16, 24.286), (-18,  0,  0.000)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((47, 14, 32.867), (-18,  0,  0.000)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((77, 12, 35.253), (-18,  0,  0.000)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), ((15,  5, 43.367), (-12, 42, 50.044)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((44, 54, 28.506), (-10, 47, 43.884)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((74, 17,  5.184), (  1,  6, 51.561)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 55,  9.138), (-10, 39, 43.554)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 37,  6.613), ( -8, 36, 43.277)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((68, 47, 25.009), (  2, 17, 23.583)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), ((53, 32,  0.497), (-18,  0,  0.000)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((83, 20,  1.540), (-18,  0,  0.000)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((66, 45, 22.460), (162,  0,  0.000)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), ((37, 18, 49.295), ( 19, 34,  7.117)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((57,  6,  0.851), ( 45,  8, 40.841)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((58, 13,  5.486), ( 95,  2, 29.439)))

            ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 7, 14,  5.521), ( 25, 48, 13.908)))
            ; (((40,  0,  0.0), (-18,  0,  0.0)), ((27, 49, 42.130), ( 32, 54, 13.184)))
            ; (((70,  0,  0.0), (-18,  0,  0.0)), ((43,  7, 36.475), ( 52,  1,  0.626)))
            ]
            |> List.map (fun ((xLat, xLng), (yLat, yLng)) ->
                let x = (DMS.FromTuple xLat, DMS.FromTuple xLng)
                let y = (DMS.FromTuple yLat, DMS.FromTuple yLng)

                (x, y))

    // The pairs of pairs of angles being the ACIC points for the direct
    // solution.
    static let ysACIC : (DMS * DMS) list =
            [ ((10, 43, 39.078), (-18,  0,  0.000))
            ; ((40, 43, 28.790), (-18,  0,  0.000))
            ; ((70, 43, 16.379), (-18,  0,  0.000))

            ; ((10, 30, 50.479), (-17, 28, 48.777))
            ; ((40, 30, 37.757), (-17, 19, 43.280))
            ; ((70, 30, 12.925), (-16, 28, 22.844))

            ; (( 9, 59, 57.087), (-17, 15, 57.926))
            ; ((39, 59, 46.211), (-17,  3, 27.942))
            ; ((69, 59, 15.149), (-15, 53, 37.449))

            ; (( 9, 38,  8.260), (-17, 21, 54.407))
            ; ((39, 31, 54.913), (-18, 43,  1.027))
            ; ((70, 42, 35.533), (-18, 22, 43.683))

            ; ((14, 21, 52.456), (-18,  0,  0.000))
            ; ((44, 20, 47.740), (-18,  0,  0.000))
            ; ((74, 19, 35.289), (-18,  0,  0.000))

            ; ((13,  4, 12.564), (-14, 51, 13.283))
            ; ((43,  0,  0.556), (-13, 48, 49.111))
            ; ((72, 47, 48.242), ( -7, 36, 58.487))

            ; (( 9, 58, 15.192), (-13, 35, 48.467))
            ; ((39, 51, 44.295), (-12, 21, 14.090))
            ; ((69, 33, 22.562), ( -5, 32,  1.822))

            ; ((17, 16, 24.286), (-18,  0,  0.000))
            ; ((47, 14, 32.867), (-18,  0,  0.000))
            ; ((77, 12, 35.253), (-18,  0,  0.000))

            ; ((15,  5, 43.367), (-12, 42, 50.044))
            ; ((44, 54, 28.506), (-10, 47, 43.884))
            ; ((74, 17,  5.184), (  1,  6, 51.561))

            ; (( 9, 55,  9.138), (-10, 39, 43.554))
            ; ((39, 37,  6.613), ( -8, 36, 43.277))
            ; ((68, 47, 25.009), (  2, 17, 23.583))

            ; ((53, 32,  0.497), (-18,  0,  0.000))
            ; ((83, 20,  1.540), (-18,  0,  0.000))
            ; ((66, 45, 22.460), (162,  0,  0.000))

            ; ((37, 18, 49.295), ( 19, 34,  7.117))
            ; ((57,  6,  0.851), ( 45,  8, 40.841))
            ; ((58, 13,  5.486), ( 95,  2, 29.439))

            ; (( 7, 14,  5.521), ( 25, 48, 13.908))
            ; ((27, 49, 42.130), ( 32, 54, 13.184))
            ; ((43,  7, 36.475), ( 52,  1,  0.626))
            ]
            |> List.map (fun (yLat, yLng) ->
                (DMS.FromTuple yLat, DMS.FromTuple yLng))

    static let es = List.replicate 39 bedfordClarke

    // The expected distances for the inverse solution from ACIC
    // aeronautical charts.
    static let ds : float<m> list =
            List.replicate 3 80466.478<m>
            @
            [ 80466.477<m>
            ]
            @ List.replicate 2 80466.478<m>
            @
            [ 80466.476<m>
            ; 80466.477<m>
            ]
            @ List.replicate 4   80466.478<m>
            @ List.replicate 9  482798.868<m>
            @ List.replicate 9  804664.780<m>
            @ List.replicate 9 4827988.683<m>

    // The forward azimuths for the direct problem.
    static let fwdAzimuths : DMS list =
            let z = 0.0<deg>

            List.replicate 3 z
            @ List.replicate 3 (45.0<deg>)
            @ List.replicate 3 (90.0<deg>)
            @ [120.0<deg>; 230.0<deg>; 350.0<deg>]
            @ List.replicate 3 z
            @ List.replicate 3 (45.0<deg>)
            @ List.replicate 3 (90.0<deg>)
            @ List.replicate 3 z
            @ List.replicate 3 (45.0<deg>)
            @ List.replicate 3 (90.0<deg>)
            @ List.replicate 3 z
            @ List.replicate 3 (45.0<deg>)
            @ List.replicate 3 (90.0<deg>)
            |> List.map DMS.FromDeg

    // The reverse azimuths from direct solution.
    static let revAzimuths : DMS list =
            let xs = List.replicate 3 (180,  0,  0.0)

            xs @
            [ (225,  5, 33.202)
            ; (225, 26,  1.695)

            // WARNING: (226, 7, 13.935) is the back azimuth and may have a typo in the
            // paper as its about 19' off. I'm replacing the expected value with the
            // one I found using Karney's method on the same ellipsoid.
            //
            // https://geographiclib.sourceforge.io/cgi-bin/GeodSolve?type=I&input=70+-18+70.50359027777777+-16.47301222222222&format=d&azi2=f&unroll=r&prec=3&radius=6378388&flattening=1%2F297&option=Submit
            // lat1 lon1 fazi1 (°) = 70°00'00.0000"N 018°00'00.0000"W 044°59'59.4176"
            // lat2 lon2 fazi2 (°) = 70°30'12.9250"N 016°28'22.8440"W 046°26'13.3521"
            //
            // lat1 lon1 fazi1 (°) = 70°00'00.0000"N 018°00'00.0000"W 044°59'59.4176"
            // lat2 lon2 bazi2 (°) = 70°30'12.9250"N 016°28'22.8440"W 226°26'13.3521"
            //
            // , (226,  7, 13.935)
            // , ( 46, 26, 13.3521)
            // , (226, 26, 13.3521)
            ; (226, 26, 13.3521)

            ; (270,  7, 38.779)
            ; (270, 36, 20.315)
            ; (271, 58, 45.080)

            ; (300,  6, 29.736)
            ; ( 49, 32, 29.011)
            ; (169, 38, 35.667)
            ]
            @ xs @
            [ (225, 37, 46.346)
            ; (227, 46, 32.221)
            ; (234, 50, 49.050)

            ; (270, 45, 49.945)
            ; (273, 37, 32.768)
            ; (281, 42, 12.088)
            ]
            @ xs @
            [ (226,  9,  1.224)
            ; (229, 52, 15.525)
            ; (243, 13, 18.356)

            ; (271, 16, 14.933)
            ; (276,  1,  6.634)
            ; (289,  1,  2.923)

            ; (180,  0,  0.0)
            ; (180,  0,  0.0)
            ; (  0,  0,  0.0)

            ; (240, 59, 37.859)
            ; (274, 57, 29.108)
            ; (332, 38, 58.143)

            ; (276, 53, 56.143)
            ; (299, 54, 41.259)
            ; (332,  0, 43.685)
            ]
            |> List.map DMS.FromTuple

    static member IndirectDistanceData : seq<obj[]>=
        List.map3 (fun e (x, y) d -> (e, x, y, d, 0.037<m>)) es xys ds
        |> Seq.map FSharpValue.GetTupleFields

    static member IndirectAzimuthFwdData : seq<obj[]>=
        List.map3
            (fun e (x, y) az -> (e, x, y, az, DMS (0<deg>, 0<min>, 30.0<s>) |> DMS.ToRad))
            es xys fwdAzimuths
        |> Seq.map FSharpValue.GetTupleFields

    static member DirectDistanceData : seq<obj[]>=
        let eds = List.zip es ds
        let xys' = List.zip xys ysACIC
        List.map3
            (fun (e, d) ((x, _), y) azFwd -> (e, x, d, azFwd, y, 1.0<m>))
            eds xys' fwdAzimuths
        |> Seq.map FSharpValue.GetTupleFields

    static member DirectAzimuthRevData : seq<obj[]>=
        let eds = List.zip es ds
        let azs = List.zip fwdAzimuths revAzimuths
        List.map3
            (fun (e, d) (x, _) (azFwd, azRev) ->
                (e, x, d, azFwd, azRev, DMS (0<deg>, 0<min>, 1.0<s>)))
            eds xys azs
        |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("IndirectDistanceData", MemberType = typeof<Bedford1978>)>]
let ``indirect solution distance from Bedford's 1978 paper`` (e, x, y, d, t) =
    let x' = LatLng.FromDMS x
    let y' = LatLng.FromDMS y
    test <@ let (TaskDistance d') = distance e x' y' in abs (d' - d) < t @>

[<Theory; MemberData("IndirectAzimuthFwdData", MemberType = typeof<Bedford1978>)>]
let ``indirect solution forward azimuth from Bedford's 1978 paper`` (e, x, y, az, t) =
    let f e x y =
        let azRad = DMS.ToRad az
        let x' = LatLng.FromDMS x
        let y' = LatLng.FromDMS y

        azimuthFwd e x' y'
        |> Option.map (fun az' -> abs (az' - azRad))

    test <@ f e x y |> function | None -> true | Some d -> d < t @>

(*
[<Theory; MemberData("DirectDistanceData", MemberType = typeof<Bedford1978>)>]
let ``direct solution distance from Bedford's 1978 paper`` (e, x, d, az, y, t) =
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

    test <@ f e x az y |> function | None -> true | Some d -> d < t @>

[<Theory; MemberData("DirectAzimuthRevData", MemberType = typeof<Bedford1978>)>]
let ``direct solution reverse azimuth from Bedford's 1978 paper`` (e, x, d, az, azBack, t) =
    let f e x az azBack =
        let azRad = DMS.ToRad az
        let azBackRad = DMS.ToRad azBack
        let x' = LatLng.FromDMS x

        (direct e defaultGeodeticAccuracy {x = x'; ``α₁`` = TrueCourse azRad; s = Radius d})
        |> (function
            | GeodeticDirect soln -> soln.``α₂`` |> Option.map (fun (TrueCourse az') -> abs (az' - azBackRad))
            | _ -> None)

    let t' = DMS.ToRad t

    test <@ f e x az azBack |> function | None -> true | Some d -> d < t' @>
*)
