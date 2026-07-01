module Cardano.Cip25Tests exposing (suite)

import Bytes.Comparable as Bytes
import Cardano.Cip25 as Cip25
import Cardano.Metadatum as Metadatum
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Dict
import Expect
import Hex
import Integer
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        minimalAssetMetadataCbor =
            EE.associativeList E.string
                E.string
                [ ( "name", "A" )
                , ( "image", "ipfs://x" )
                ]

        validPolicyIdHex =
            "590f6d119b214cdcf7ef7751f8b7f1de615ff8f6de097a5ce62b257b"

        validPolicyId =
            Bytes.fromHexUnchecked validPolicyIdHex

        validAssetName =
            Bytes.fromText "A"

        v2PayloadWithAssetMetadata version policyId assetName assetMetadata =
            EE.associativeList identity
                identity
                [ ( E.string "version", version )
                , ( Bytes.toCbor policyId
                  , EE.associativeList identity
                        identity
                        [ ( Bytes.toCbor assetName, assetMetadata ) ]
                  )
                ]

        v2Payload version policyId assetName =
            v2PayloadWithAssetMetadata version policyId assetName minimalAssetMetadataCbor

        v1PayloadWithAssetKey assetNameKey =
            EE.associativeList identity
                identity
                [ ( E.string "version", E.string "1" )
                , ( E.string validPolicyIdHex
                  , EE.associativeList identity
                        identity
                        [ ( assetNameKey, minimalAssetMetadataCbor ) ]
                  )
                ]

        expectDecodeFailure payload =
            payload
                |> E.encode
                |> D.decode Cip25.fromCbor
                |> Expect.equal Nothing
    in
    describe "Cip25"
        [ test "v2 cip25 payload round trips through CBOR" <|
            \_ ->
                case ( Cip25.imageMimeFromString "image/png", Cip25.file "manifest" "application/json" "ipfs://manifest" ) of
                    ( Just imageMime, Just file ) ->
                        let
                            policyId =
                                Bytes.fromHexUnchecked "70b582d6ae2f720a329f7320b1034b5a077415a0b7e37fd994bf7060"

                            assetName =
                                Bytes.fromText "Tappys"

                            assetMetadata =
                                { name = String.repeat 40 "é"
                                , image = Cip25.Image "ipfs://image"
                                , mediaType = Just imageMime
                                , description = Just (String.repeat 40 "é")
                                , files = [ { file | otherProps = Dict.singleton "sha256" (Metadatum.String "abc123") } ]
                                , otherProps = Dict.singleton "rarity" (Metadatum.String "common")
                                }
                        in
                        case Cip25.singleton policyId assetName assetMetadata of
                            Just cip25 ->
                                cip25
                                    |> Cip25.toCbor
                                    |> E.encode
                                    |> D.decode Cip25.fromCbor
                                    |> Maybe.andThen (Cip25.getAssetMetadata policyId assetName)
                                    |> Expect.equal (Just assetMetadata)

                            Nothing ->
                                Expect.fail "Expected valid policy id and asset name"

                    _ ->
                        Expect.fail "Expected MIME types to be valid"
        , test "reserved asset and file fields cannot be shadowed by otherProps" <|
            \_ ->
                case ( Cip25.imageMimeFromString "image/png", Cip25.file "manifest" "application/json" "ipfs://manifest" ) of
                    ( Just imageMime, Just file ) ->
                        let
                            fileWithReservedProps =
                                { file
                                    | otherProps =
                                        Dict.fromList
                                            [ ( "name", Metadatum.String "wrong" )
                                            , ( "mediaType", Metadatum.String "text/plain" )
                                            , ( "src", Metadatum.String "ipfs://wrong" )
                                            , ( "sha256", Metadatum.String "abc123" )
                                            ]
                                }

                            assetMetadata =
                                { name = "A"
                                , image = Cip25.Image "ipfs://x"
                                , mediaType = Just imageMime
                                , description = Just "description"
                                , files = [ fileWithReservedProps ]
                                , otherProps =
                                    Dict.fromList
                                        [ ( "name", Metadatum.String "wrong" )
                                        , ( "image", Metadatum.String "ipfs://wrong" )
                                        , ( "mediaType", Metadatum.String "text/plain" )
                                        , ( "description", Metadatum.String "wrong" )
                                        , ( "files", Metadatum.String "wrong" )
                                        , ( "rarity", Metadatum.String "common" )
                                        ]
                                }

                        in
                        case Cip25.singleton validPolicyId validAssetName assetMetadata of
                            Just cip25 ->
                                let
                                    expectedFile =
                                        { file | otherProps = Dict.singleton "sha256" (Metadatum.String "abc123") }

                                    expected =
                                        { assetMetadata
                                            | files = [ expectedFile ]
                                            , otherProps = Dict.singleton "rarity" (Metadatum.String "common")
                                        }
                                in
                                cip25
                                    |> Cip25.toCbor
                                    |> E.encode
                                    |> D.decode Cip25.fromCbor
                                    |> Maybe.andThen (Cip25.getAssetMetadata validPolicyId validAssetName)
                                    |> Expect.equal (Just expected)

                            Nothing ->
                                Expect.fail "Expected valid policy id and asset name"

                    _ ->
                        Expect.fail "Expected MIME types to be valid"
        , test "cip25 decodes v1 cbor with string version one" <|
            \_ ->
                let
                    payload =
                        EE.associativeList identity
                            identity
                            [ ( E.string "version", E.string "1" )
                            , ( E.string validPolicyIdHex
                              , EE.associativeList E.string
                                    identity
                                    [ ( "A"
                                      , minimalAssetMetadataCbor
                                      )
                                    ]
                              )
                            ]

                    decoded =
                        payload
                            |> E.encode
                            |> D.decode Cip25.fromCbor
                in
                case decoded of
                    Just cip25 ->
                        Expect.all
                            [ \_ ->
                                cip25
                                    |> Cip25.getAssetMetadata validPolicyId validAssetName
                                    |> Expect.equal (Just (Cip25.assetMetadata "A" (Cip25.Image "ipfs://x")))
                            , \_ ->
                                cip25
                                    |> Cip25.toCbor
                                    |> E.encode
                                    |> Hex.fromBytes
                                    |> Expect.equal
                                        (payload
                                            |> E.encode
                                            |> Hex.fromBytes
                                        )
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected synthesized v1 payload to decode"
        , test "cip25 rejects policy ids with invalid length" <|
            \_ ->
                v2Payload
                    (E.int 2)
                    (Bytes.fromHexUnchecked "590f6d119b214cdcf7ef7751f8b7f1de615ff8f6de097a5ce62b25")
                    validAssetName
                    |> expectDecodeFailure
        , test "cip25 rejects asset names over 32 bytes" <|
            \_ ->
                v2Payload
                    (E.int 2)
                    validPolicyId
                    (Bytes.fromText (String.repeat 33 "a"))
                    |> expectDecodeFailure
        , test "v1 cip25 rejects non-text asset name keys" <|
            \_ ->
                v1PayloadWithAssetKey (Bytes.toCbor validAssetName)
                    |> expectDecodeFailure
        , test "cip25 rejects v2 string versions" <|
            \_ ->
                v2Payload
                    (E.string "2.0")
                    validPolicyId
                    validAssetName
                    |> expectDecodeFailure
        , test "cip25 rejects invalid asset metadata fields" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        v2PayloadWithAssetMetadata
                            (E.int 2)
                            validPolicyId
                            validAssetName
                            (EE.associativeList E.string
                                E.string
                                [ ( "image", "ipfs://x" ) ]
                            )
                            |> expectDecodeFailure
                    , \_ ->
                        v2PayloadWithAssetMetadata
                            (E.int 2)
                            validPolicyId
                            validAssetName
                            (EE.associativeList E.string
                                E.string
                                [ ( "name", "A" ) ]
                            )
                            |> expectDecodeFailure
                    , \_ ->
                        v2PayloadWithAssetMetadata
                            (E.int 2)
                            validPolicyId
                            validAssetName
                            (EE.associativeList E.string
                                E.string
                                [ ( "name", "A" )
                                , ( "image", "ipfs://x" )
                                , ( "mediaType", "application/json" )
                                ]
                            )
                            |> expectDecodeFailure
                    ]
                    ()
        , test "implicit v1 cip25 payload round trips through CBOR" <|
            \_ ->
                let
                    -- From mainnet tx hash:
                    -- c5cac452cde5166bfa0bb4ce71ca39827103d62bcadee3f1fad8f143619127aa
                    payload =
                        "a178383730623538326436616532663732306133323966373332306231303334623561303737343135613062376533376664393934626637303630a166546170707973a66158781c68747470733a2f2f747769747465722e636f6d2f546170546f6f6c73646e616d656654617070797365696d6167657835697066733a2f2f516d57646648616746414e384b4533376750676643374a48593970416e6e6d4e735137563831387162716354337a667469636b65726554415050596757656273697465781868747470733a2f2f7777772e746170746f6f6c732e696f2f6b6465736372697074696f6e783b546170546f6f6c73204d656d65636f696e206861732046696e616c6c79204172726976656421204e46412e204d656d657320617265205269736b79"

                    decoded =
                        payload
                            |> Hex.toBytesUnchecked
                            |> D.decode Cip25.fromCbor
                in
                case decoded of
                    Just cip25 ->
                        let
                            policyId =
                                Bytes.fromHexUnchecked "70b582d6ae2f720a329f7320b1034b5a077415a0b7e37fd994bf7060"

                            assetName =
                                Bytes.fromText "Tappys"

                            expected =
                                { name = "Tappys"
                                , image = Cip25.Image "ipfs://QmWdfHagFAN8KE37gPgfC7JHY9pAnnmNsQ7V818qbqcT3z"
                                , mediaType = Nothing
                                , description = Just "TapTools Memecoin has Finally Arrived! NFA. Memes are Risky"
                                , files = []
                                , otherProps =
                                    Dict.fromList
                                        [ ( "Website", Metadatum.String "https://www.taptools.io/" )
                                        , ( "X", Metadatum.String "https://twitter.com/TapTools" )
                                        , ( "ticker", Metadatum.String "TAPPY" )
                                        ]
                                }
                        in
                        Expect.all
                            [ \_ ->
                                cip25
                                    |> Cip25.getAssetMetadata policyId assetName
                                    |> Expect.equal (Just expected)
                            , \_ ->
                                cip25
                                    |> Cip25.toCbor
                                    |> E.encode
                                    |> Hex.fromBytes
                                    |> Expect.equal payload
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected fixture to decode"
        , test "cip25 decodes v1 cbor with legacy string version" <|
            \_ ->
                let
                    -- From mainnet tx hash:
                    -- 22a49994886458267382b8d488874855584cac0b0284d0fd1f8d49373a89aec7
                    payload =
                        "a26776657273696f6e63312e3078383539306636643131396232313463646366376566373735316638623766316465363135666638663664653039376135636536326232353762a165534841524ca9646e616d656f534841524c204855534b454e53414e6566696c657381a3637372637835697066733a2f2f516d65744c5161443576767273584338784e685073416655335278397a317241505a5a4c69766f50715677537271646e616d6565534841524c696d656469615479706569696d6167652f706e6765696d6167657835697066733a2f2f516d65744c5161443576767273584338784e685073416655335278397a317241505a5a4c69766f507156775372716673796d626f6c65534841524c676d696e74696e67a364747970657074696d652d6c6f636b2d706f6c6963796a626c6f636b636861696e6763617264616e6f766d696e7465644265666f7265536c6f744e756d6265721a070b5247696d656469615479706569696d6167652f706e6769746f6b656e5479706565746f6b656e6b6465736372697074696f6e70436172646f6e7a6f20466f756e6465726b746f74616c537570706c791b000000e8990a4600"

                    decoded =
                        payload
                            |> Hex.toBytesUnchecked
                            |> D.decode Cip25.fromCbor
                in
                case decoded of
                    Just cip25 ->
                        let
                            policyId =
                                Bytes.fromHexUnchecked "590f6d119b214cdcf7ef7751f8b7f1de615ff8f6de097a5ce62b257b"

                            assetName =
                                Bytes.fromText "SHARL"

                            expected =
                                { name = "SHARL HUSKENSAN"
                                , image = Cip25.Image "ipfs://QmetLQaD5vvrsXC8xNhPsAfU3Rx9z1rAPZZLivoPqVwSrq"
                                , mediaType = Cip25.imageMimeFromString "image/png"
                                , description = Just "Cardonzo Founder"
                                , files =
                                    Cip25.file "SHARL" "image/png" "ipfs://QmetLQaD5vvrsXC8xNhPsAfU3Rx9z1rAPZZLivoPqVwSrq"
                                        |> Maybe.map List.singleton
                                        |> Maybe.withDefault []
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
                        in
                        Expect.all
                            [ \_ ->
                                cip25
                                    |> Cip25.getAssetMetadata policyId assetName
                                    |> Expect.equal (Just expected)
                            , \_ ->
                                cip25
                                    |> Cip25.toCbor
                                    |> E.encode
                                    |> Hex.fromBytes
                                    |> Expect.equal payload
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected fixture to decode"
        ]
