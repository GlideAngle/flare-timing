module Flight.Haversine

open System
open Flight.Units.Angle
open Flight.Units.DegMinSec
open Flight.LatLng
open Flight.Zone
open Flight.Geodesy
open Flight.Geodesy.Problem
open Flight.Earth.Sphere

let haversine (x : float<rad>) : float<rad> =
    let y = sin (float x / 2.0) in y * y * 1.0<rad>

let aOfHaversine {Lat = xLatF; Lng = xLngF} {Lat = yLatF; Lng = yLngF} =
    let (dLatF : float<rad>, dLngF : float<rad>) = (yLatF - xLatF, yLngF - xLngF)
    let hLatF = haversine dLatF
    let hLngF = haversine dLngF

    hLatF
    + cos (float xLatF)
    * cos (float yLatF)
    * hLngF

// Spherical distance using haversines and floating point numbers.
let distance (x : LatLng) (y : LatLng) : TaskDistance =
    let (Radius rEarth) = earthRadius
    let radDist : float<1> = 2.0 * asin (sqrt (float (aOfHaversine x y)))
    radDist * rEarth |> TaskDistance

let azimuthFwd' {Lat = xLat; Lng = xLng} {Lat = yLat; Lng = yLng} : float<rad> =
    let xLat' = float xLat
    let xLng' = float xLng
    let yLat' = float yLat
    let yLng' = float yLng

    let deltaLng = yLng' - xLng'
    let x = sin deltaLng * cos yLat'
    let y = cos xLat' * sin yLat' - sin xLat' * cos yLat' * cos deltaLng

    atan2 x y * 1.0<rad>

let azimuthFwd x y = azimuthFwd' x y |> Some

let azimuthRev x y =
    azimuthFwd y x
    |> Option.map (fun az -> let (Rad x) = Rad.Rotate (Math.PI * 1.0<rad> |> Rad) (Rad az) in x)

let direct 
    (prob : DirectProblem<LatLng, TrueCourse, Radius>)
    : DirectSolution<LatLng, TrueCourse> =
    let ``_Φ₁`` = float prob.x.Lat
    let ``_L₁`` = float prob.x.Lng
    let ``α₁`` = let (TrueCourse x) = prob.``α₁`` in float x
    let (Radius earthR) = earthRadius
    let (Radius d) = prob.s
    let dR = d / earthR

    // SEE: https://www.movable-type.co.uk/scripts/latlong.html
    let ``_Φ₂`` = asin (sin ``_Φ₁`` * cos(dR) + cos ``_Φ₁`` * sin (dR) * cos ``α₁``)
    let ``_L₂`` = ``_L₁`` + atan2 (sin ``α₁`` * sin (dR) * cos ``_Φ₁``) (cos (dR) - sin ``_Φ₁`` * sin ``_Φ₂``)
    let y = {Lat = ``_Φ₂`` * 1.0<rad>; Lng = ``_L₂`` * 1.0<rad>}

    { y = y
    ; ``α₂`` =
        azimuthFwd y prob.x
        |> Option.map (Rad.FromRad >> Rad.Rotate (Rad <| Math.PI * 1.0<rad>) >> Rad.Normalize >> Rad.ToRad >> TrueCourse)
    }

let inverse
    ({ x = {Lat = ``_Φ₁``; Lng = ``_L₁``} as x; y = {Lat = ``_Φ₂``; Lng = ``_L₂``} as y} : InverseProblem<LatLng>)
    : InverseSolution<TaskDistance,float<rad>> =
    { s = distance x y
    ; ``α₁`` = azimuthFwd' x y
    ; ``α₂`` = azimuthRev x y
    }

module HaversineTests =
    open Xunit
    open Hedgehog

    [<Fact>]
    let ``haversines is not negative`` () = Property.check <| property {
            let! x = Gen.double <| Range.constantBounded ()
            return haversine (x * 1.0<rad>) >= 0.0<rad>
        }