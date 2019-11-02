module Flight.LatLng

open System
open Flight.Units
open Flight.Units.Angle
open Flight.Units.DegMinSec

type LatLng =
    {Lat : float<rad>; Lng : float<rad>}
    override x.ToString() =
        let f = DMS.FromRad >> string
        sprintf "(%s, %s)" (f x.Lat) (f x.Lng)

type TaskDistance = TaskDistance of float<m>

/// A function for measuring the distance between two points given as latitude
/// longitude pairs in radians.
type SpanLatLng = LatLng -> LatLng -> TaskDistance

/// A function calculating the forward azimuth between two points given as
/// latitude longitude pairs in radians.
type AzimuthFwd = LatLng -> LatLng -> float<rad> option

/// A function calculating the reverse azimuth between two points given as
/// latitude longitude pairs in radians.
type AzimuthRev = LatLng -> LatLng -> float<rad> option

/// Makes a LatLng with normalized latitude and longitude.
let mkLatLng (lat : float<rad>) (lng : float<rad>) : LatLng option =
    if Double.IsNaN (float lat) then None
    elif Double.IsNaN (float lng) then None
    else
        lat
        |> Rad.FromRad
        |> Rad.Normalize
        |> Rad.PlusMinusHalfPi
        |> Option.map (fun (Rad lat') ->
            let (Rad lng') = lng |> Rad.FromRad |> Rad.Normalize |> Rad.PlusMinusPi
            if Double.IsNaN (float lat') then None
            elif Double.IsNaN (float lng') then None
            else Some {Lat = lat'; Lng = lng'})
        |> Option.bind id