module Flight.Units.Angle

open System

[<Measure>] type rad
[<Measure>] type deg

let degToRad (x : float<deg>) : float<rad> =
    x * Math.PI / 180.0<deg/rad>
let radToDeg (x : float<rad>) : float<deg> =
    x / Math.PI * 180.0<deg/rad>  