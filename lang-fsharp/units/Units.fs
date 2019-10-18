module Flight.Units

open System

[<Measure>] type rad
[<Measure>] type deg
[<Measure>] type km

let degToRad (x : float<deg>) : float<rad> =
    Math.PI * x / 180.0<deg/rad>