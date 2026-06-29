module Cardano.Cip25Tests exposing (suite)

import Cardano.Cip25 as Cip25
import Cbor.Decode as D
import Cbor.Encode as E
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cip25"
        [ test "strings round trip through CBOR chunks" <|
            \_ ->
                let
                    string =
                        String.repeat 40 "é"
                in
                Cip25.stringToCbor string
                    |> E.encode
                    |> D.decode Cip25.stringFromCbor
                    |> Expect.equal (Just string)
        ]
