module Earth

open Units
type Location = { Latitude : float<deg>; Longitude : float<deg> }

let GreatCircleDistance<[<Measure>] 'u> (R : float<'u>) (p1 : Location) (p2 : Location) =
    let degToRad (x : float<deg>) = System.Math.PI * x / 180.0<deg/rad>

    let sq x = x * x
    // take the sin of the half and square the result
    let sinSqHf (a : float<rad>) = (System.Math.Sin >> sq) (a / 2.0<rad>)
    let cos (a : float<deg>) = System.Math.Cos (degToRad a / 1.0<rad>)

    let dLat = (p2.Latitude - p1.Latitude) |> degToRad
    let dLon = (p2.Longitude - p1.Longitude) |> degToRad

    let a = sinSqHf dLat + cos p1.Latitude * cos p2.Latitude * sinSqHf dLon
    let c = 2.0 * System.Math.Atan2(System.Math.Sqrt(a), System.Math.Sqrt(1.0-a))

    R * c

let GreatCircleDistanceOnEarth = GreatCircleDistance 6371.0<km>

let xs =
    [|
        [| -33.36137; 147.93207; -33.85373; 147.94195 |]
        [| -33.85373; 147.94195; -33.4397; 148.34533 |]
        [| -33.4397; 148.34533; -33.61965; 148.4099 |]
    |]

for x in xs do
    match x with
    | [| xLat; xLng; yLat; yLng |] ->
        let p1 = { Latitude = xLat * 1.0<deg>; Longitude = xLng * 1.0<deg> }
        let p2 = { Latitude = yLat * 1.0<deg>; Longitude = yLng * 1.0<deg> }
        let d = GreatCircleDistanceOnEarth p1 p2
        printfn "%A" d
    | _ -> printfn "Bad input, expecting array of arrays, each inner array four elements."