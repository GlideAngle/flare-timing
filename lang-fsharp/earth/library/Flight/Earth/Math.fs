module Flight.Earth.Math

let cos2 cos' (σ1 : float) (σ : float) : float * float =
    let ``_2σm`` = 2.0 * σ1 + σ
    let cos2σm = cos' ``_2σm``
    (cos2σm, cos2σm * cos2σm)
