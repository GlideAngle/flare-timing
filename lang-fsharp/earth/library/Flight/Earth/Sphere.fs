module Flight.Earth.Sphere

open Flight.Units
open Flight.Zone

let earthRadius = Radius 6371000.0<m>

module Internal =

    open System
    open Flight.Units.Angle
    open Flight.Units.DegMinSec
    open Flight.LatLng

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

    let azimuthFwd {Lat = xLat; Lng = xLng} {Lat = yLat; Lng = yLng} : float<rad> option =
        let xLat' = float xLat
        let xLng' = float xLng
        let yLat' = float yLat
        let yLng' = float yLng

        let deltaLng = yLng' - xLng'
        let x = sin deltaLng * cos yLat'
        let y = cos xLat' * sin yLat' - sin xLat' * cos yLat' * cos deltaLng

        Some <| atan2 x y * 1.0<rad>

    let azimuthRev x y =
        azimuthFwd y x
        |> Option.map (fun az -> let (Rad x) = Rad.Rotate (Math.PI * 1.0<rad> |> Rad) (Rad az) in x)

    module SphereTests =
        open Xunit
        open Hedgehog

        [<Fact>]
        let ``haversines is not negative`` () = Property.check <| property {
                let! x = Gen.double <| Range.constantBounded ()
                return haversine (x * 1.0<rad>) >= 0.0<rad>
            }