module Internal.Sphere

open System
open Flight.Units
open Flight.Units.Angle
open Flight.Units.Convert
open Flight.LatLng
open Flight.Zone
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

module SphereTests =
    open Xunit
    open Hedgehog

    [<Fact>]
    let ``haversines is not negative`` () = Property.check <| property {
            let! x = Gen.double <| Range.constantBounded ()
            return haversine (x * 1.0<rad>) >= 0.0<rad>
        }