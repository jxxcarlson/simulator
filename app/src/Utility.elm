module Utility exposing (applyToList, iterate, randomElement)

import List.Extra
import Random


{-|

    > applyToList (\a b -> a + b) [1,2,3] 0
    --> Just 6

-}
applyToList : (a -> b -> b) -> List a -> b -> Maybe b
applyToList f list b0 =
    let
        step : ( List a, Maybe b ) -> ( List a, Maybe b )
        step ( list_, b_ ) =
            let
                b__ =
                    Maybe.map2 f (List.head list_) b_
            in
            ( List.drop 1 list_, b__ )
    in
    iterate (List.length list) step ( list, Just b0 )
        |> Tuple.second


{-|

    > iterate 3 (\x -> 2*x) 1
    8 : number

-}
iterate : Int -> (a -> a) -> a -> a
iterate n f x0 =
    if n == 0 then
        x0

    else
        iterate (n - 1) f (f x0)


{-| Return a random random element of a list, along with a new seed
-}
randomElement : Random.Seed -> List a -> ( Maybe a, Random.Seed )
randomElement seed list =
    let
        ( x, newSeed ) =
            Random.step probability seed
    in
    ( elementAtX x list, newSeed )


{-| Let p be a number between 0 and 1. Subdivide the
interval [0, 1] into n subintervals. Let k be the
index of the subinterval containing p. Return k.
Here k a non-negative integer less than n.
-}
indexOfX : Float -> Int -> Int
indexOfX p n =
    p * toFloat n |> floor |> clamp 0 (n - 1)


elementAtX : Float -> List a -> Maybe a
elementAtX x list =
    if x < 0 || x > 1 then
        Nothing

    else
        List.Extra.getAt (indexOfX x (List.length list)) list


probability : Random.Generator Float
probability =
    Random.float 0 1
