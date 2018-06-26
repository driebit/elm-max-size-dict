module MaxSizeDict
    exposing
        ( MaxSizeDict
        , empty
        , get
        , insert
        , isEmpty
        , maxSize
        , member
        , remove
        , singleton
        , size
        )

{-| A dictionary with a maximum size. Items are removed from the dictionary on a
FIFO basis when the dictionary reaches its maximum size.


# Dictionaries

@docs MaxSizeDict


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, get, size, maxSize

-}

import Dict exposing (Dict)
import Fifo exposing (Fifo)


{-| A dictionary with a maximum size.
-}
type MaxSizeDict k v
    = MaxSizeDict
        { dict : Dict k v
        , maxSize : Int
        , keys : Fifo k
        }



-- Build


{-| Create an empty dictionary.
-}
empty : Int -> MaxSizeDict k v
empty maxSize =
    MaxSizeDict
        { dict = Dict.empty
        , maxSize = maxSize
        , keys = Fifo.empty
        }


{-| Create a singleton dictionary.
-}
singleton : Int -> comparable -> v -> MaxSizeDict comparable v
singleton maxSize key value =
    MaxSizeDict
        { dict = Dict.singleton key value
        , maxSize = maxSize
        , keys = Fifo.insert key Fifo.empty
        }


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert :
    comparable
    -> v
    -> MaxSizeDict comparable v
    -> MaxSizeDict comparable v
insert key value ((MaxSizeDict dict) as maxSizeDict) =
    if member key maxSizeDict then
        let
            keys =
                removeKey key dict.keys |> Fifo.insert key
        in
        MaxSizeDict
            { dict
                | dict = Dict.insert key value dict.dict
                , keys = keys
            }
    else if size maxSizeDict == maxSize maxSizeDict then
        case Fifo.remove dict.keys of
            -- should NOT happen
            ( Nothing, _ ) ->
                maxSizeDict

            ( Just oldest, keys ) ->
                MaxSizeDict
                    { dict
                        | dict =
                            Dict.remove oldest dict.dict
                                |> Dict.insert key value
                        , keys = Fifo.insert key keys
                    }
    else
        MaxSizeDict
            { dict
                | dict = Dict.insert key value dict.dict
                , keys = Fifo.insert key dict.keys
            }


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> MaxSizeDict comparable v -> MaxSizeDict comparable v
remove key ((MaxSizeDict dict) as maxSizeDict) =
    if member key maxSizeDict then
        MaxSizeDict
            { dict
                | dict = Dict.remove key dict.dict
                , keys = removeKey key dict.keys
            }
    else
        maxSizeDict



-- Query


{-| Determine if a dictionary is empty.
-}
isEmpty : MaxSizeDict k v -> Bool
isEmpty (MaxSizeDict dict) =
    Dict.isEmpty dict.dict


{-| Determine the maximum number of key-value pairs in a dictionary.
-}
maxSize : MaxSizeDict k v -> Int
maxSize (MaxSizeDict dict) =
    dict.maxSize


{-| Determine the number of key-value pairs in a dictionary.
-}
size : MaxSizeDict k v -> Int
size (MaxSizeDict dict) =
    Dict.size dict.dict


{-| Determine if a key is in a dictionary.
-}
member : comparable -> MaxSizeDict comparable v -> Bool
member key (MaxSizeDict dict) =
    Dict.member key dict.dict


{-| Get the value associated with a key. If the key is not found, return
`Nothing`.
-}
get : comparable -> MaxSizeDict comparable v -> Maybe v
get key (MaxSizeDict dict) =
    Dict.get key dict.dict



-- Internal functions


removeKey : comparable -> Fifo comparable -> Fifo comparable
removeKey key keys =
    Fifo.toList keys |> List.filter (\k -> k /= key) |> Fifo.fromList
