module Tests exposing (..)

import Expect exposing (Expectation)
import Fifo
import MaxSizeDict as Dict
import Test exposing (..)


fst ( x, y ) =
    x


all : Test
all =
    describe
        "MaxSizeDict"
        [ test "should return empty dict" <|
            \() ->
                Dict.empty 10
                    |> Dict.isEmpty
                    |> Expect.equal True
        , test "should return max size" <|
            \() ->
                Dict.empty 10
                    |> Dict.maxSize
                    |> Expect.equal 10
        , test "should return size" <|
            \() ->
                Dict.empty 10
                    |> Dict.size
                    |> Expect.equal 0
        , test "should return singleton dict" <|
            \() ->
                Dict.singleton 10 1 "one"
                    |> (\d ->
                            ( Dict.size d, Dict.maxSize d )
                       )
                    |> Expect.equal ( 1, 10 )
        , test "should return true" <|
            \() ->
                Dict.singleton 10 1 "one"
                    |> Dict.member 1
                    |> Expect.equal True
        , test "should return true after insert" <|
            \() ->
                Dict.empty 10
                    |> Dict.insert 1 "one"
                    |> Dict.member 1
                    |> Expect.equal True
        , test "should return false after insert b/c max size" <|
            \() ->
                Dict.empty 1
                    |> Dict.insert 1 "one"
                    |> Dict.insert 2 "two"
                    |> Dict.member 1
                    |> Expect.equal False
        , test "should return true after insert b/c insert is newest" <|
            \() ->
                Dict.empty 1
                    |> Dict.insert 1 "one"
                    |> Dict.insert 2 "two"
                    |> Dict.insert 1 "one"
                    |> Dict.member 1
                    |> Expect.equal True
        , test "should return true after remove" <|
            \() ->
                Dict.empty 1
                    |> Dict.insert 1 "one"
                    |> Dict.remove 1
                    |> Dict.member 1
                    |> Expect.equal False
        , test "get should return Just ..." <|
            \() ->
                Dict.empty 1
                    |> Dict.insert 1 "one"
                    |> Dict.get 1
                    |> Expect.equal (Just "one")
        ]
