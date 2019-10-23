module Flight.LatLng
open Flight.Units.Angle
open Flight.Units

type LatLng = {Lat : float<rad>; Lng : float<rad>}
type TaskDistance = TaskDistance of float<m>

// A function for measuring the distance between two points given as latitude
// longitude pairs in radians.
type SpanLatLng = LatLng -> LatLng -> TaskDistance

// A function calculating the forward azimuth between two points given as
// latitude longitude pairs in radians.
type AzimuthFwd = LatLng -> LatLng -> float<rad> option

// A function calculating the reverse azimuth between two points given as
// latitude longitude pairs in radians.
type AzimuthRev = LatLng -> LatLng -> float<rad> option