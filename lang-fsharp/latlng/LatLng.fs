module Flight.LatLng
open Flight.Units.Angle
open Flight.Units

type LatLng = {Lat : float<rad>; Lng : float<rad>}
type TaskDistance = TaskDistance of float<m>

// A function for measuring the distance between two points given as latitude
// longitude pairs in radians.
type SpanLatLng = LatLng -> LatLng -> TaskDistance