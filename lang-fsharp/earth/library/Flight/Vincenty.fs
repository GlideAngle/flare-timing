module Flight.Vincenty

open System
open Flight.Units
open Flight.Units.Angle
open Flight.Units.Convert
open Flight.Zone
open Flight.LatLng
open Flight.Units.DegMinSec
open Flight.Earth.Ellipsoid
open Flight.Geodesy
open Flight.Geodesy.Problem

let tooFar = TaskDistance 20000000.0<m>

let rec inverse
    (ellipsoid : Ellipsoid)
    accuracy
    ({ x = {Lat = ``_Φ₁``; Lng = ``_L₁``}; y = {Lat = ``_Φ₂``; Lng = ``_L₂``}} : InverseProblem<LatLng>)
    : GeodeticInverse<InverseSolution<TaskDistance,float<rad>>> =

    let (Radius a) = ellipsoid.equatorialR
    let (Radius b) = ellipsoid.PolarRadius
    let f = ellipsoid.Flattening

    let auxLat = atan << (fun x -> (1.0 - f) * x) << tan
    let ``_U₁`` = auxLat (float ``_Φ₁``)
    let ``_U₂`` = auxLat (float ``_Φ₂``)
    let ``_L`` =
        match ``_L₂`` - ``_L₁`` with
        | ``_L'`` | ``_L'`` when abs (float ``_L'``) <= Math.PI -> ``_L'``
        | _ -> Math.normalizeLng ``_L₂`` - Math.normalizeLng ``_L₁``

    let ``sinU₁`` = sin ``_U₁``
    let ``sinU₂`` = sin ``_U₂``
    let ``cosU₁`` = cos ``_U₁``
    let ``cosU₂`` = cos ``_U₂``
    let ``sinU₁sinU₂`` = ``sinU₁`` * ``sinU₂``
    let ``cosU₁cosU₂`` = ``cosU₁`` * ``cosU₂``

    let rec loop (λ : float<rad>) : GeodeticInverse<InverseSolution<_, float<rad>>> =
        let sinλ = sin (float λ)
        let cosλ = cos (float λ)

        // WARNING: The sign of numerator and denominator are important
        // in atan2. The sign of each term below follows Vincenty's
        // 1975 paper "Direct and Inverse Solutions of Geodesics on the
        // Ellipsoid with Application of Nested Equations"
        //
        // i' = cosU₁ * sinλ
        // j' = -sinU₁ * cosU₂ + cosU₁ * sinU₂ * cosλ
        //
        // By contrast Delorme's 1978 paper "Evaluation Direct and
        // Inverse Geodetic Algorithms" has this formulation with the
        // signs reversed.
        //
        // i' = -cosU₁ * sinλ
        // j' = sinU₁ * cosU₂ - cosU₁ * sinU₂ * cosλ
        //
        // As the method is Vincenty's I'm going their signage.
        let i' = ``cosU₁`` * sinλ
        let j' = -``sinU₁`` * ``cosU₂`` + ``cosU₁`` * ``sinU₂`` * cosλ

        let i = ``cosU₂`` * sinλ
        let j = ``cosU₁`` * ``sinU₂`` - ``sinU₁`` * ``cosU₂`` * cosλ

        let ``sin²σ`` = i * i + j * j
        let sinσ = sqrt ``sin²σ``
        let cosσ = ``sinU₁sinU₂`` + ``cosU₁cosU₂`` * cosλ

        let σ = atan2 sinσ cosσ

        let sinα = ``cosU₁cosU₂`` * sinλ / sinσ
        let ``cos²α`` = 1.0 - sinα * sinα
        let _C = f / 16.0 * ``cos²α`` * (4.0 + f * (4.0 - 3.0 * ``cos²α``))
        let ``u²`` = let ``b²`` = b * b in ``cos²α`` * (a * a - ``b²``) / ``b²``

        // NOTE: Start and end points on the equator, _C = 0.
        let cos2σm = if ``cos²α`` = 0.0 then 0.0 else cosσ - 2.0 * ``sinU₁sinU₂`` / ``cos²α``
        let ``cos²2σm`` = cos2σm * cos2σm

        let _A = 1.0 + ``u²`` / 16384.0 * (4096.0 + ``u²`` * (-768.0 + ``u²`` * (320.0 - 175.0 * ``u²``)))
        let _B = ``u²`` / 1024.0 * (256.0 + ``u²`` * (-128.0 + ``u²`` * (74.0 - 47.0 * ``u²``)))

        let y =
            cosσ * (-1.0 + 2.0 * ``cos²2σm``)
            - _B / 6.0 * cos2σm * (-3.0 + 4.0 * ``sin²σ``) * (-3.0 + 4.0 * ``cos²2σm``)

        let _Δσ = _B * sinσ * (cos2σm + _B / 4.0 * y)

        let x = cos2σm + _C * cosσ * (-1.0 + 2.0 * ``cos²2σm``)
        let λ' = float _L + (1.0 - _C) * f * sinα * (σ + _C * sinσ * x)

        let (GeodeticAccuracy tolerance) = accuracy

        if abs (float λ) > Math.PI then GeodeticInverseAntipodal
        elif abs ((float λ) - λ') >= tolerance then loop (λ' * 1.0<rad>)
        else
            { s = TaskDistance <| b * _A * (σ - _Δσ)
            ; ``α₁`` =
                atan2 i j * 1.0<rad>
                |> (Rad.FromRad >> Rad.Normalize >> Rad.ToRad)
            ; ``α₂`` =
                atan2 i' j' * 1.0<rad>
                |> (Rad.FromRad >> Rad.Normalize >> Rad.ToRad >> Some)
            }
            |> GeodeticInverse

    loop _L

and distanceUnchecked ellipsoid prob : GeodeticInverse<InverseSolution<TaskDistance,float<rad>>> =
    let
        { x = {Lat = xLat; Lng = xLng}
        ; y = {Lat = yLat; Lng = yLng}
        } = prob

    let minBound = System.Double.MinValue * 1.0<rad>
    let maxBound = System.Double.MaxValue * 1.0<rad>

    if prob.x = prob.y then
        GeodeticInverse <|
            { s = TaskDistance 0.0<m>
            ; ``α₁`` = 0.0<rad>
            ; ``α₂`` = Some <| Math.PI * 1.0<rad>
            }

    elif xLat < minBound then failwith "xlat under" // GeodeticInverseAbnormal LatUnder
    elif xLat > maxBound then failwith "xlat over" // GeodeticInverseAbnormal LatOver
    elif xLng < minBound then failwith "xlng under" // GeodeticInverseAbnormal LngUnder
    elif xLng > maxBound then failwith "xlng over" // GeodeticInverseAbnormal LngOver

    elif yLat < minBound then failwith "ylat under" // GeodeticInverseAbnormal LatUnder
    elif yLat > maxBound then failwith "ylat over" // GeodeticInverseAbnormal LatOver
    elif yLng < minBound then failwith "ylng under" // GeodeticInverseAbnormal LngUnder
    elif yLng > maxBound then failwith "ylng over" // GeodeticInverseAbnormal LngOver
    else inverse ellipsoid defaultGeodeticAccuracy prob

let distance e (x : LatLng) (y : LatLng) =
    let {Lat = qx} = x
    let {Lat = qy} = y
    let {Lat = xLat; Lng = xLng} = x
    let {Lat = yLat; Lng = yLng} = y

    match (Rad.PlusMinusHalfPi (Rad xLat), Rad.PlusMinusHalfPi (Rad yLat)) with
    | (None, _) | (_, None) ->
        failwith
        <| sprintf
                "Latitude of %s or %s is outside -90° .. 90° range"
                (string <| DMS.FromRad qx)
                (string <| DMS.FromRad qy)

    | (Some (Rad xLat'), Some (Rad yLat')) ->
        let (Rad xLng') = Rad.PlusMinusPi (Rad xLng)
        let (Rad yLng') = Rad.PlusMinusPi (Rad yLng)

        let x' = {Lat = xLat'; Lng = xLng'}
        let y' = {Lat = yLat'; Lng = yLng'}

        match distanceUnchecked e {x = x'; y = y'} with
        | GeodeticInverseAntipodal -> tooFar
        | GeodeticInverseAbnormal _ -> tooFar
        | GeodeticInverse x -> x.s

let azimuthFwd (e : Ellipsoid) (x : LatLng) (y : LatLng) : float<rad> option =
    match distanceUnchecked e {x = x; y = y} with
    | GeodeticInverseAntipodal -> None
    | GeodeticInverseAbnormal _ -> None
    | GeodeticInverse x -> Some x.``α₁``

let azimuthRev (e : Ellipsoid) (x : LatLng) (y : LatLng) : float<rad> option =
    match distanceUnchecked e {x = x; y = y} with
    | GeodeticInverseAntipodal -> None
    | GeodeticInverseAbnormal _ -> None
    | GeodeticInverse x -> x.``α₂``

module VincentyTests =
    open FSharp.Reflection
    open FSharp.Data.UnitSystems.SI.UnitSymbols
    open Xunit
    open Swensen.Unquote
    open Hedgehog

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

    type Bedford1978 () =
        // The pairs of pairs of angles being the input points for the inverse
        // problem.
        static let xys : ((DMS * DMS) * (DMS * DMS)) list =
                [ (((10,  0,  0.0), (-18,  0,  0.0)), ((10, 43, 39.078), (-18,  0,  0.0)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((40, 43, 28.790), (-18,  0,  0.0)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 43, 16.379), (-18,  0,  0.0)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), ((10, 30, 50.497), (-17, 28, 48.777)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((40, 30, 37.757), (-17, 19, 43.280)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 30, 12.925), (-16, 28, 22.844)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 59, 57.087), (-17, 15, 57.926)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 59, 46.211), (-17,  3, 27.942)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((69, 59, 15.149), (-15, 53, 37.449)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 38,  8.260), (-17, 21, 54.407)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 31, 54.913), (-18, 43,  1.027)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((70, 42, 35.533), (-18, 22, 43.683)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), ((14, 21, 52.456), (-18,  0,  0.0)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((44, 20, 47.740), (-18,  0,  0.0)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((74, 19, 35.289), (-18,  0,  0.0)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), ((13,  4, 12.564), (-14, 51, 13.283)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((43,  0,  0.556), (-13, 48, 49.111)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((72, 47, 48.242), ( -7, 36, 58.487)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 58, 15.192), (-13, 35, 48.467)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 51, 44.295), (-12, 21, 14.090)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((69, 33, 22.562), ( -5, 32,  1.822)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), ((17, 16, 24.286), (-18,  0,  0.0)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((47, 14, 32.867), (-18,  0,  0.0)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((77, 12, 35.253), (-18,  0,  0.0)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), ((15,  5, 43.367), (-12, 42, 50.044)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((44, 54, 28.506), (-10, 47, 43.884)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((74, 17,  5.184), (  1,  6, 51.561)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), (( 9, 55,  9.138), (-10, 39, 43.554)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((39, 37,  6.613), ( -8, 36, 43.277)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((68, 47, 25.009), (  2, 17, 23.583)))

                ; (((10,  0,  0.0), (-18,  0,  0.0)), ((53, 32,  0.497), (-18,  0,  0.0)))
                ; (((40,  0,  0.0), (-18,  0,  0.0)), ((83, 20,  1.540), (-18,  0,  0.0)))
                ; (((70,  0,  0.0), (-18,  0,  0.0)), ((66, 45, 22.460), (162,  0,  0.0)))

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
                @ List.replicate 4 80466.478<m>
                @ List.replicate 9 482798.868<m>
                @ List.replicate 9 804664.780<m>
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

        static member DistanceData : seq<obj[]>=
            List.map3 (fun e (x, y) d -> (e, x, y, d, 0.037<m>)) es xys ds
            |> Seq.map FSharpValue.GetTupleFields

        static member AzimuthFwdData : seq<obj[]>=
            List.map3 (fun e (x, y) az -> (e, x, y, az, DMS (0<deg>, 0<min>, 30.0<s>) |> DMS.ToRad)) es xys fwdAzimuths
            |> Seq.map FSharpValue.GetTupleFields

        static member AzimuthRevData : seq<obj[]>=
            List.map3 (fun e (x, y) az -> (e, x, y, az, DMS (0<deg>, 0<min>, 0.016667<s>) |> DMS.ToRad)) es xys revAzimuths
            |> Seq.map FSharpValue.GetTupleFields

    [<Theory; MemberData("DistanceData", MemberType = typeof<Bedford1978>)>]
    let ``distances from Bedford's 1978 paper`` (e, x, y, d, t) =
        let x' = LatLng.FromDMS x
        let y' = LatLng.FromDMS y
        test <@ let (TaskDistance d') = distance e x' y' in abs (d' - d) < t @>

    [<Theory; MemberData("AzimuthFwdData", MemberType = typeof<Bedford1978>)>]
    let ``forward azimuth from Bedford's 1978 paper`` (e, x, y, az, t) =
        let f e x y =
            let azRad = DMS.ToRad az
            let x' = LatLng.FromDMS x
            let y' = LatLng.FromDMS y

            azimuthFwd e x' y'
            |> Option.map (fun az' -> abs (az' - azRad))

        test <@ f e x y |> function | None -> true | Some d -> d < t @>

(* WARNING: This test is not setup properly. The test data azimuths are
computed reverse azimuths for the solution to the direct problem, not the
inverse problem.

    [<Theory; MemberData("AzimuthRevData", MemberType = typeof<Bedford1978>)>]
    let ``reverse azimuth from Bedford's 1978 paper`` (e, x, y, az, t) =
        let f e x y =
            let azRad = DMS.ToRad az
            let x' = LatLng.FromDMS x
            let y' = LatLng.FromDMS y

            azimuthRev e x' y'
            |> Option.map (fun az' -> abs (az' - azRad))

        test <@ f e x y |> function | None -> true | Some d -> d < t @>
        *)
