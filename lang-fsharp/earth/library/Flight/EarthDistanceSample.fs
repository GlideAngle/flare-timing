module Flight.EarthDistanceSample

open System
open Flight.Units
open Flight.Units.Angle
open Flight.Units.Convert
open Flight.LatLng

let distance<[<Measure>] 'u> (R : float<'u>) (p1 : LatLng) (p2 : LatLng) : float<'u> =
    let sq x = x * x
    // take the sin of the half and square the result
    let sinSqHf (a : float<rad>) = (Math.Sin >> sq) (a / 2.0<rad>)
    let cos (a : float<rad>) = Math.Cos (float a)

    let dLat = (p2.Lat - p1.Lat)
    let dLon = (p2.Lng - p1.Lng)

    let a = sinSqHf dLat + cos p1.Lat * cos p2.Lat * sinSqHf dLon
    let c = 2.0 * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1.0 - a))

    R * c

let xs =
    [|
        [| -33.36137; 147.93207; -33.85373; 147.94195 |]
        [| -33.85373; 147.94195; -33.4397; 148.34533 |]
        [| -33.4397; 148.34533; -33.61965; 148.4099 |]
    |]
    |> Array.map (Array.map (((*) 1.0<deg>) >> convertDegToRad))

for x in xs do
    match x with
    | [| xLat; xLng; yLat; yLng |] ->
        let p1 = {Lat = xLat; Lng = xLng}
        let p2 = {Lat = yLat; Lng = yLng}
        let d = distance 6371.0<km> p1 p2
        printfn "%A" d
    | _ -> printfn "Bad input, expecting array of arrays, each inner array four elements."