module Cardano.Cip25Tests exposing (suite)

import Cardano.Cip25 as Cip25
import Cardano.Metadatum as Metadatum
import Cbor.Decode as D
import Cbor.Encode as E
import Dict
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
        , test "files round trip through CBOR" <|
            \_ ->
                case Cip25.mimeTypeFromString "application/json" of
                    Nothing ->
                        Expect.fail "Expected application/json to be a valid MIME type"

                    Just mediaType ->
                        let
                            file =
                                { name = String.repeat 40 "é"
                                , mediaType = mediaType
                                , src = "ipfs://example"
                                , otherProps = Dict.singleton "sha256" (Metadatum.String "abc123")
                                }
                        in
                        file
                            |> Cip25.fileToCbor
                            |> E.encode
                            |> D.decode Cip25.fileFromCbor
                            |> Expect.equal (Just file)
        ]
