module Cardano.Cip25Tests exposing (suite)

import Bytes.Comparable as Bytes
import Cardano.Cip25 as Cip25
import Cardano.Metadatum as Metadatum
import Cbor.Decode as D
import Cbor.Encode as E
import Dict
import Expect
import Hex
import Integer
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
        , test "asset metadata round trips through CBOR" <|
            \_ ->
                case ( Cip25.imageMimeFromString "image/png", Cip25.mimeTypeFromString "application/json" ) of
                    ( Just imageMime, Just fileMime ) ->
                        let
                            assetMetadata =
                                { name = String.repeat 40 "é"
                                , image = Cip25.Image "ipfs://image"
                                , mediaType = Just imageMime
                                , description = Just "description"
                                , files =
                                    [ { name = "manifest"
                                      , mediaType = fileMime
                                      , src = "ipfs://manifest"
                                      , otherProps = Dict.singleton "sha256" (Metadatum.String "abc123")
                                      }
                                    ]
                                , otherProps = Dict.singleton "rarity" (Metadatum.String "common")
                                }
                        in
                        assetMetadata
                            |> Cip25.assetMetadataToCbor
                            |> E.encode
                            |> D.decode Cip25.assetMetadataFromCbor
                            |> Expect.equal (Just assetMetadata)

                    _ ->
                        Expect.fail "Expected MIME types to be valid"
        , test "cip25 decodes v1 cbor with legacy string version" <|
            \_ ->
                case Cip25.file "SHARL" "image/png" "ipfs://QmetLQaD5vvrsXC8xNhPsAfU3Rx9z1rAPZZLivoPqVwSrq" of
                    Just file ->
                        let
                            policyId =
                                Bytes.fromHexUnchecked "590f6d119b214cdcf7ef7751f8b7f1de615ff8f6de097a5ce62b257b"

                            assetName =
                                Bytes.fromText "SHARL"

                            payload =
                                "a26776657273696f6e63312e3078383539306636643131396232313463646366376566373735316638623766316465363135666638663664653039376135636536326232353762a165534841524ca9646e616d656f534841524c204855534b454e53414e6566696c657381a3637372637835697066733a2f2f516d65744c5161443576767273584338784e685073416655335278397a317241505a5a4c69766f50715677537271646e616d6565534841524c696d656469615479706569696d6167652f706e6765696d6167657835697066733a2f2f516d65744c5161443576767273584338784e685073416655335278397a317241505a5a4c69766f507156775372716673796d626f6c65534841524c676d696e74696e67a364747970657074696d652d6c6f636b2d706f6c6963796a626c6f636b636861696e6763617264616e6f766d696e7465644265666f7265536c6f744e756d6265721a070b5247696d656469615479706569696d6167652f706e6769746f6b656e5479706565746f6b656e6b6465736372697074696f6e70436172646f6e7a6f20466f756e6465726b746f74616c537570706c791b000000e8990a4600"

                            expected =
                                { name = "SHARL HUSKENSAN"
                                , image = Cip25.Image "ipfs://QmetLQaD5vvrsXC8xNhPsAfU3Rx9z1rAPZZLivoPqVwSrq"
                                , mediaType = Cip25.imageMimeFromString "image/png"
                                , description = Just "Cardonzo Founder"
                                , files = [ file ]
                                , otherProps =
                                    Dict.fromList
                                        [ ( "minting"
                                          , Metadatum.Map
                                                [ ( Metadatum.String "type", Metadatum.String "time-lock-policy" )
                                                , ( Metadatum.String "blockchain", Metadatum.String "cardano" )
                                                , ( Metadatum.String "mintedBeforeSlotNumber", Metadatum.Int (Integer.fromSafeInt 118182471) )
                                                ]
                                          )
                                        , ( "symbol", Metadatum.String "SHARL" )
                                        , ( "tokenType", Metadatum.String "token" )
                                        , ( "totalSupply", Metadatum.Int (Integer.fromSafeString "999000000000") )
                                        ]
                                }

                            decoded =
                                payload
                                    |> Hex.toBytesUnchecked
                                    |> D.decode Cip25.fromCbor
                        in
                        case decoded of
                            Just cip25 ->
                                cip25
                                    |> Cip25.getAssetMetadata policyId assetName
                                    |> Expect.equal (Just expected)

                            Nothing ->
                                Expect.fail "Expected fixture to decode"

                    Nothing ->
                        Expect.fail "Expected image/png file metadata to be valid"
        ]
