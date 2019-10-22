module Flight.Units.Convert

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Flight.Units
open Flight.Units.Angle

let convertMinToSec (min : float<min>) : float<s> = min * 60.0<s/min>
let convertDegToSec (deg : float<deg>) : float<s> = deg * 3600.0<s/deg>

let convertDegToMin (deg : float<deg>) : float<min> = deg * 60.0<min/deg>

let convertMinToDeg (min : float<min>) : float<deg> = min / 60.0<min/deg>
let convertSecToDeg (s : float<s>) : float<deg> = s / 3600.0<s/deg>

let convertDegToRad (x : float<deg>) : float<rad> = x * Math.PI / 180.0<deg/rad>
let convertRadToDeg (x : float<rad>) : float<deg> = x / Math.PI * 180.0<deg/rad>  