module Statistics exposing (center, mean, roundTo, stats, stdev, variance)


mean : List Float -> Float
mean data =
    List.sum data / (toFloat <| List.length data)


center : List Float -> List Float
center data =
    let
        m =
            mean data
    in
    List.map (\datum -> datum - m) data


variance : List Float -> Float
variance data =
    data
        |> center
        |> List.map (\x -> x * x)
        |> List.sum
        |> (\x -> x / toFloat (List.length data))


stdev : List Float -> Float
stdev data =
    sqrt (variance data)


roundTo : Int -> Float -> Float
roundTo d x =
    let
        factor =
            10.0 ^ toFloat d
    in
    x
        * factor
        |> round
        |> toFloat
        |> (\u -> u / factor)


stats : List Float -> { n : Int, mean : Float, stdev : Float }
stats data =
    { n = List.length data, mean = mean data |> roundTo 1, stdev = stdev data |> roundTo 1 }
