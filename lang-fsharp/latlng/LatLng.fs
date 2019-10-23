module Flight.LatLng
open Flight.Units.Angle
open Flight.Units

type LatLng = {Lat : float<rad>; Lng : float<rad>}
type TaskDistance = TaskDistance of float<m>