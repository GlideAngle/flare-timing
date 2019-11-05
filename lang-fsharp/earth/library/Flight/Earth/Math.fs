module Flight.Earth.Math

let cos2 (σ1 : float) (σ : float) : float * float =
    let cos2σm = cos (2.0 * σ1 + σ)
    (cos2σm, cos2σm * cos2σm)
