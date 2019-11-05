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
open Flight.Earth.Math

let tooFar = TaskDistance 20000000.0<m>

let auxLat (f : float) : float -> float = atan << (fun x -> (1.0 - f) * x) << tan

let rec iterateAngularDistance
    accuracy
    _A
    _B
    s
    b
    σ1
    σ =
    let (GeodeticAccuracy tolerance) = accuracy
    let (cos2σm, ``cos²2σm``) = cos2 σ1 σ
    let sinσ = sin σ
    let cosσ = cos σ
    let ``sin²σ`` = sinσ * sinσ

    let _Δσ =
            _B * sinσ *
                (cos2σm + _B / 4. *
                    (cosσ * (-1. + 2. * ``cos²2σm``)
                        - _B / 6.
                            * cos2σm
                            * (-3. + 4. * ``sin²σ``)
                            * (-3. + 4. * ``cos²2σm``)))

    let σ' = s / (b * _A) + _Δσ
    if abs (σ - σ') < tolerance
        then σ
        else
            iterateAngularDistance accuracy _A _B s b σ1 σ'

// The solution to the direct geodesy problem with input latitude rejected
// outside the range -90° .. 90° and longitude normalized to -180° .. 180°.
let rec direct
    (e : Ellipsoid)
    (a : GeodeticAccuracy)
    ({x = {Lat = xLat; Lng = xLng}; ``α₁`` = TrueCourse qTC} as p : DirectProblem<LatLng, TrueCourse, Radius>)
    : GeodeticDirect<DirectSolution<LatLng, TrueCourse>> =

    match Rad.PlusMinusHalfPi (Rad xLat) with
    | None ->
        failwith
        <| sprintf
                "Latitude of %s is outside -90° .. 90° range"
                (string <| DMS.FromRad xLat)

    | Some (Rad nLat) ->
        let (Rad nLng) = Rad.PlusMinusPi (Rad xLng)
        let xNorm = {Lat = nLat; Lng = nLng}

        let (Rad nTC) = Rad.Normalize (Rad qTC)
        let tcNorm = TrueCourse nTC

        directUnchecked e a {p with x = xNorm; ``α₁`` = tcNorm}

// The solution to the direct geodesy problem with input latitude unchecked and
// longitude not normalized.
//
// Symbol reference from Vincenty's paper.
// a, b   = major and minor semiaxes of the ellipsoid
// f      = flattening (a - b) / a
// Φ      = geodetic latitude, positive north of the equator
// L      = difference in longitude, positive east
// s      = length of the geodesic
// α₁, α₂ = azimuths o the geodesic, clockwise rom the north: α₂ in the direction P₁ P₂ produced
// α      = azimuth of the geodesic at the equator
// U      = reduced latitude, defined by tan U = (1 - f) tan Φ1
// λ      = difference in longitude on an auxillary sphere
// σ      = angular distance P₁ P₂, on the sphere
// σ1     = angular distance on the sphere from the equator to P₁
// σm     = angular distance on the sphere rom the equator to the midpoint of the line
and directUnchecked
    (ellipsoid : Ellipsoid)
    (accuracy : GeodeticAccuracy)
    (prob : DirectProblem<LatLng, TrueCourse, Radius>)
    : GeodeticDirect<DirectSolution<LatLng,TrueCourse>> =
    let
        { x = {Lat = Φ1; Lng = λ1}
        ; ``α₁`` = TrueCourse α1
        ; s = Radius s
        } = prob

    let (Radius a) = ellipsoid.equatorialR
    let (Radius b) = ellipsoid.PolarRadius
    let f = ellipsoid.Flattening

    // Initial setup
    let _U1 = auxLat f (float Φ1)
    let cosU1 = cos _U1
    let sinU1 = sin _U1

    // NOTE: In some transcriptions of Vincenty's formula to code the following
    // are sometimes seen for calculating U1, cosU1 and sinU1.
    // let tanU1 = (1. - f) * tan (float Φ1)
    // let cosU1 = 1. / sqrt (1. + tanU1 * tanU1)
    // let sinU1 = tanU1 * cosU1
    //
    // SEE: https://www.purplemath.com/modules/idents.htm
    // secx = 1 / cosx
    // tan²x + 1 = sec²x
    // tanx = sinx / cosx

    let cosα1 = cos (float α1)
    let sinα1 = sin (float α1)
    let σ1 = atan2 (tan _U1) cosα1

    let sinα = cosU1 * sinα1
    let ``sin²α`` = sinα * sinα
    let ``cos²α`` = 1. - ``sin²α``

    let ``u²`` =
        let ``a²`` = a * a
        let ``b²`` = b * b
        ``cos²α`` * (``a²`` - ``b²``) / ``b²``

    let _A = 1.0 + ``u²`` / 16384. * (4096. + ``u²`` * (-768. + ``u²`` * (320. - 175. * ``u²``)))
    let _B = ``u²`` / 1024. * (256. + ``u²`` * (-128. + ``u²`` * (74. - 47. * ``u²``)))

    // Solution
    let σ = iterateAngularDistance accuracy _A _B s b σ1 (s / (b * _A))

    let sinσ = sin σ
    let cosσ = cos σ

    let v = sinU1 * cosσ + cosU1 * sinσ * cosα1

    let (j, j') =
        let sinU1sinσ = sinU1 * sinσ
        let cosU1cosσcosα1 = cosU1 * cosσ * cosα1
        (   sinU1sinσ  - cosU1cosσcosα1
        , -(sinU1sinσ) + cosU1cosσcosα1
        )

    let w = (1. - f) * sqrt (``sin²α`` + j * j)
    let Φ2 = atan2 v w * 1.<rad>
    let λ = atan2 (sinσ * sinα1) (cosU1 * cosσ - sinU1 * sinσ * cosα1)
    let _C = f / 16. * ``cos²α`` * (4. + f * (4. - 3. * ``cos²α``))

    let _L =
        let (cos2σm, ``cos²2σm``) = cos2 σ1 σ
        let y = cos2σm + _C * cosσ * (-1. + 2. * ``cos²2σm``)
        let x = σ + _C * sinσ * y
        λ - (1. - _C) * f * sinα * x

    let _L2 = _L * 1.<rad> + λ1

    GeodeticDirect
        { y = {Lat = Φ2; Lng = _L2}
        ; ``α₂`` =
            atan2 sinα j' * 1.<rad>
            |> (Rad.FromRad >> Rad.Normalize >> Rad.ToRad >> TrueCourse >> Some)
        }

let rec inverse
    (ellipsoid : Ellipsoid)
    accuracy
    ({ x = {Lat = ``_Φ₁``; Lng = ``_L₁``}; y = {Lat = ``_Φ₂``; Lng = ``_L₂``}} : InverseProblem<LatLng>)
    : GeodeticInverse<InverseSolution<TaskDistance,float<rad>>> =

    let (Radius a) = ellipsoid.equatorialR
    let (Radius b) = ellipsoid.PolarRadius
    let f = ellipsoid.Flattening

    let ``_U₁`` = auxLat f (float ``_Φ₁``)
    let ``_U₂`` = auxLat f (float ``_Φ₂``)
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
        let ``cos²α`` = 1. - sinα * sinα
        let _C = f / 16. * ``cos²α`` * (4. + f * (4. - 3. * ``cos²α``))
        let ``u²`` = let ``b²`` = b * b in ``cos²α`` * (a * a - ``b²``) / ``b²``

        // NOTE: Start and end points on the equator, _C = 0.
        let cos2σm = if ``cos²α`` = 0. then 0. else cosσ - 2. * ``sinU₁sinU₂`` / ``cos²α``
        let ``cos²2σm`` = cos2σm * cos2σm

        let _A = 1. + ``u²`` / 16384. * (4096. + ``u²`` * (-768. + ``u²`` * (320. - 175. * ``u²``)))
        let _B = ``u²`` / 1024. * (256. + ``u²`` * (-128. + ``u²`` * (74. - 47. * ``u²``)))

        let y =
            cosσ * (-1. + 2. * ``cos²2σm``)
            - _B / 6. * cos2σm * (-3. + 4. * ``sin²σ``) * (-3. + 4. * ``cos²2σm``)

        let _Δσ = _B * sinσ * (cos2σm + _B / 4. * y)

        let x = cos2σm + _C * cosσ * (-1. + 2. * ``cos²2σm``)
        let λ' = float _L + (1. - _C) * f * sinα * (σ + _C * sinσ * x)

        let (GeodeticAccuracy tolerance) = accuracy

        if abs (float λ) > Math.PI then GeodeticInverseAntipodal
        elif abs ((float λ) - λ') >= tolerance then loop (λ' * 1.<rad>)
        else
            { s = TaskDistance <| b * _A * (σ - _Δσ)
            ; ``α₁`` =
                atan2 i j * 1.<rad>
                |> (Rad.FromRad >> Rad.Normalize >> Rad.ToRad)
            ; ``α₂`` =
                atan2 i' j' * 1.<rad>
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
            { s = TaskDistance 0.<m>
            ; ``α₁`` = 0.<rad>
            ; ``α₂`` = Some <| Math.PI * 1.<rad>
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
