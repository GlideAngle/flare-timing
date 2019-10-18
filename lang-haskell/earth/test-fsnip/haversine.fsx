// SEE: http://www.fssnip.net/jp/title/Great-circle-distance
open System

module Earth =
    type Point(lon,lat) = 
        let toRad deg = deg * (Math.PI / 180.0)
        
        member this.Lon = lon
        member this.Lat = lat
        
        member this.LonRad = toRad lon
        member this.LatRad = toRad lat

    
    let greatCircleDistance (p1:Point) (p2:Point) =
        // code adapted from 
        // http://www.codeproject.com/Articles/12269/Distance-between-locations-using-latitude-and-long
        (*
            The Haversine formula according to Dr. Math.
            http://mathforum.org/library/drmath/view/51879.html
                
            dlon = lon2 - lon1
            dlat = lat2 - lat1
            a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
            c = 2 * atan2(sqrt(a), sqrt(1-a)) 
            d = R * c
                
            Where
                * dlon is the change in longitude
                * dlat is the change in latitude
                * c is the great circle distance in Radians.
                * R is the radius of a spherical Earth.
                * The locations of the two points in 
                    spherical coordinates (longitude and 
                    latitude) are lon1,lat1 and lon2, lat2.
        *)
        
        let dlon = p2.LonRad - p1.LonRad;
        let dlat = p2.LatRad - p1.LatRad;

        // Intermediate result a.
        let a = (sin (dlat / 2.0)) ** 2.0 + ((cos p1.LatRad) * (cos p2.LatRad) * (sin (dlon / 2.0)) ** 2.0);

        // Intermediate result c (great circle distance in Radians).
        let c = 2.0 * (asin (sqrt a));
        let c' = 2.0 * (atan (sqrt a));
        let c'' = 2.0 * (atan2 (sqrt a) (sqrt <| 1.0 - a));

        // Distance.
        let earthRadiusKms = 6371.0;
        let distance = earthRadiusKms * c;
        let distance' = earthRadiusKms * c';
        let distance'' = earthRadiusKms * c'';

        (distance, distance', distance'')

    let test = 
        let xs =
            [|
                [| -33.36137; 147.93207; -33.85373; 147.94195 |]
                [| -33.85373; 147.94195; -33.4397; 148.34533 |]
                [| -33.4397; 148.34533; -33.61965; 148.4099 |]
            |]

        for x in xs do
            match x with
            | [| xLat; xLng; yLat; yLng |] ->
                let d, d', d'' = greatCircleDistance (new Point(xLat, xLng)) (new Point(yLat, yLng))
                printfn "asin => %f, atan => %f, atan2 => %f" d d' d''
            | _ -> printfn "Bad input, expecting array of arrays, each inner array four elements."

//asin => 46.409922, atan => 46.409615, atan2 => 46.409922
//asin => 59.505739, atan => 59.505091, atan2 => 59.505739
//asin => 18.489526, atan => 18.489507, atan2 => 18.489526