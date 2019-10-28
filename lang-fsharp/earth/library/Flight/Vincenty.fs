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
            ; ``α₁`` = atan2 i j * 1.0<rad>
            ; ``α₂`` = Some <| atan2 i' j' * 1.0<rad>
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
    // fromMaybe (error msg) $ do
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

let azimuthFwd e x y =
    match distanceUnchecked e {x = x; y = y} with
    | GeodeticInverseAntipodal -> None
    | GeodeticInverseAbnormal _ -> None
    | GeodeticInverse x -> Some x.``α₁``

let azimuthRev e x y =
    match distanceUnchecked e {x = x; y = y} with
    | GeodeticInverseAntipodal -> None
    | GeodeticInverseAbnormal _ -> None
    | GeodeticInverse x -> x.``α₂``

module VincentyTests =
    open FSharp.Reflection
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
    type VincentyData () =
        static member Data =
            [
                (
                    { Lat = DMS.FromTuple ( 55, 45,  0.0) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (  0,  0,  0.0) |> DMS.ToRad
                    }
                ,
                    { Lat = DMS.FromTuple (-33, 26,  0.0) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (108, 13,  0.0) |> DMS.ToRad
                    }
                , 14110526.170<m>
                , bessel
                , 0.001<m>
                )
            ;
                (
                    { Lat = DMS.FromTuple ( 37, 19, 54.95367) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (  0,  0,  0.0) |> DMS.ToRad
                    }
                ,
                    { Lat = DMS.FromTuple ( 26,  7, 42.83946) |> DMS.ToRad
                    ; Lng = DMS.FromTuple ( 41, 28, 35.50729) |> DMS.ToRad
                    }
                , 4085966.703<m>
                , hayford
                , 0.001<m>
                )
            ;
                (
                    { Lat = DMS.FromTuple ( 35, 16, 11.24862) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (  0,  0,  0.0) |> DMS.ToRad
                    }
                ,
                    { Lat = DMS.FromTuple ( 67, 22, 14.77638) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (137, 47, 28.31435) |> DMS.ToRad
                    }
                , 8084823.839<m>
                , hayford
                , 0.001<m>
                )
            ;
                (
                    { Lat = DMS.FromTuple (  1,  0,  0.0) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (  0,  0,  0.0) |> DMS.ToRad
                    }
                ,
                    { Lat = DMS.FromTuple ( 0, -59, 53.83076) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (179, 17, 48.02997) |> DMS.ToRad
                    }
                , 19960000.000<m>
                , hayford
                , 0.001<m>
                )
            ;
                (
                    { Lat = DMS.FromTuple (  1,  0,  0.0) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (  0,  0,  0.0) |> DMS.ToRad
                    }
                ,
                    { Lat = DMS.FromTuple (  1,  1, 15.18952) |> DMS.ToRad
                    ; Lng = DMS.FromTuple (179, 46, 17.84244) |> DMS.ToRad
                    }
                , 19780006.5584<m>
                , hayford
                , 0.001<m>
                )
            ]
            |> Seq.map FSharpValue.GetTupleFields

    [<Theory; MemberData("Data", MemberType = typeof<VincentyData>)>]
    let ``distances from Vincenty's 1975 paper`` (x, y, d, e, t) =
        test <@ let (TaskDistance d') = distance e x y in abs (d' - d) < t @>
