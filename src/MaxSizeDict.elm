module MaxSizeDict
    exposing
        ( empty
        , get
        , insert
        , isEmpty
        , maxSize
        , member
        , remove
        , singleton
        , size
        )

import Dict exposing (Dict)
import Fifo exposing (Fifo)


type alias MaxSizeDict k v =
    { dict : Dict k v
    , maxSize : Int
    , keys : Fifo k
    }



-- Build


empty : Int -> MaxSizeDict k v
empty maxSize =
    { dict = Dict.empty
    , maxSize = maxSize
    , keys = Fifo.empty
    }


singleton : Int -> comparable -> v -> MaxSizeDict comparable v
singleton maxSize key value =
    { dict = Dict.singleton key value
    , maxSize = maxSize
    , keys = Fifo.insert key Fifo.empty
    }


insert :
    comparable
    -> v
    -> MaxSizeDict comparable v
    -> MaxSizeDict comparable v
insert key value dict =
    if member key dict then
        let
            keys =
                removeKey key dict.keys |> Fifo.insert key
        in
        { dict
            | dict = Dict.insert key value dict.dict
            , keys = keys
        }
    else if size dict == maxSize dict then
        case Fifo.remove dict.keys of
            -- should NOT happen
            ( Nothing, _ ) ->
                dict

            ( Just oldest, keys ) ->
                { dict
                    | dict =
                        Dict.remove oldest dict.dict
                            |> Dict.insert key value
                    , keys = Fifo.insert key keys
                }
    else
        { dict
            | dict = Dict.insert key value dict.dict
            , keys = Fifo.insert key dict.keys
        }


remove : comparable -> MaxSizeDict comparable v -> MaxSizeDict comparable v
remove key dict =
    if member key dict then
        { dict
            | dict = Dict.remove key dict.dict
            , keys = removeKey key dict.keys
        }
    else
        dict



-- Query


isEmpty : MaxSizeDict k v -> Bool
isEmpty dict =
    Dict.isEmpty dict.dict


maxSize : MaxSizeDict k v -> Int
maxSize =
    .maxSize


size : MaxSizeDict k v -> Int
size dict =
    Dict.size dict.dict


member : comparable -> MaxSizeDict comparable v -> Bool
member key dict =
    Dict.member key dict.dict


get : comparable -> MaxSizeDict comparable v -> Maybe v
get key dict =
    Dict.get key dict.dict



-- Internal functions


removeKey : comparable -> Fifo comparable -> Fifo comparable
removeKey key keys =
    Fifo.toList keys |> List.filter (\k -> k /= key) |> Fifo.fromList
