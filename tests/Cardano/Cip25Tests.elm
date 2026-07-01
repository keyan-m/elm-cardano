module Cardano.Cip25Tests exposing (suite)

import Bytes.Comparable as Bytes
import Cardano.Cip25 as Cip25
import Cardano.Metadatum as Metadatum
import Cardano.Transaction as Transaction
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

        tappyPolicyId =
            Bytes.fromHexUnchecked "70b582d6ae2f720a329f7320b1034b5a077415a0b7e37fd994bf7060"

        tappyAssetName =
            Bytes.fromText "Tappys"

        tappyAssetMetadata =
            { name = "Tappys"
            , image = "ipfs://QmWdfHagFAN8KE37gPgfC7JHY9pAnnmNsQ7V818qbqcT3z"
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
                                , image = "ipfs://image"
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
                                , image = "ipfs://x"
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
                                    |> Expect.equal (Just (Cip25.assetMetadata "A" "ipfs://x"))
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
                            expected =
                                tappyAssetMetadata
                        in
                        Expect.all
                            [ \_ ->
                                cip25
                                    |> Cip25.getAssetMetadata tappyPolicyId tappyAssetName
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
        , test "full tappy mint transaction decodes to expected CIP-25 asset metadata" <|
            \_ ->
                let
                    -- From mainnet tx hash:
                    -- c5cac452cde5166bfa0bb4ce71ca39827103d62bcadee3f1fad8f143619127aa
                    transactionCbor =
                        "84a8008282582041358d287c0f22bc27f990328a82cd8611324eb25b8291ae7ddc8a412cb71e7300825820a6db9e862491d94b3ef18ccf1563077816e2b3250e8d5198094823f53a572df800018a825839016d3156743424f083398f738e04bcbdc09ae4b763208671ecc80d9aa0ff0e3778025a3047e7078cd8fdbd664b6075bcf6e540b4e8ae2ee177821a001e8480a1581c70b582d6ae2f720a329f7320b1034b5a077415a0b7e37fd994bf7060a1465461707079731a04a2cb718258390129f47adc43bd16f83d0614154c989c26363c9aed9ed3e546879e01dbe18669c66dd13a9b9c2b8ea15057721a73565934a4ebd7e0732869b91a00e35b20825839018675f3a8e93151cd20f7a52716b41fd0f878ab3946515146d566187f86ed649ef147b52dcff2001bd9fdb10966cebb507ae7f593e81e9d481a00970fe082583901baa811b0f50a0d57192b5799f7263b184a89c54195065236fc7cb36c37e0b9013a56d9c56e02d8800ab9c725f119ee4123bbeaf62e5b3c931a00adf340825839012e5110c5cb8fd23cad54698e20daa624aba53b8054be029b31311fdec0b8c884f8d189d88b4d10ca714bd5f5e3ed2fc7b8da6a554cc0e5f81a00a7d8c0825839017461f652e575739c812dcfd2ca0efe664d33816aff02651714011167c0b8c884f8d189d88b4d10ca714bd5f5e3ed2fc7b8da6a554cc0e5f81a00249f008258390129f47adc43bd16f83d0614154c989c26363c9aed9ed3e546879e01dbe18669c66dd13a9b9c2b8ea15057721a73565934a4ebd7e0732869b91a015752a082583901baa811b0f50a0d57192b5799f7263b184a89c54195065236fc7cb36c37e0b9013a56d9c56e02d8800ab9c725f119ee4123bbeaf62e5b3c931a015752a082583901300d7f8c6fd21de6d7f251525025504ca9178544653cf38a4fc86e64154b32845dbaa695ade475eed65b69fb24bb3f85b3f09c99ab0c1cb91a01c9c38082581d6196b2259f6c695459cdff6d5b7c0a0226a596dc4b4a8d43c1d031087b1a0794a407021a00035439031a074de0000758205dcbd1890bb064085531ee06fd12b17ccb1b013af728529efed899117f9dcf5c081a074ddb5009a1581c70b582d6ae2f720a329f7320b1034b5a077415a0b7e37fd994bf7060a1465461707079731a04a2cb710e81581c96b2259f6c695459cdff6d5b7c0a0226a596dc4b4a8d43c1d031087ba2008282582087ed0ab1d2e0e0b7e4e1dbcbea78bd07d547218173f3ef5948f3940d089427bc5840db0a328a026f3e6cbb301d341b9bbf6732c853ef07788f63d8914376062b56666ad9a9e4b4cdcc8f39e178d7e0f6189b4bedb8ce0b1d27a1667613f485776a0e825820a9bc92ad8beb04a691c3d21197064f4a2041128da020c0128588d43d139b166d5840cc9ec58a374c5dad97c98339966fe12c894bce43638d8484c8589dec9ca1af60af43afbb75668902f17e5be7e968daeb7e0150005fe94b1af20fdfa53934ab0701818201828200581cbcc60eba87230c5d3c2af1f16ad9aeabc2481fff0a01cab7f0f88f7b82051a074de0c8f5d90103a100a11902d1a178383730623538326436616532663732306133323966373332306231303334623561303737343135613062376533376664393934626637303630a166546170707973a66b6465736372697074696f6e783b546170546f6f6c73204d656d65636f696e206861732046696e616c6c79204172726976656421204e46412e204d656d657320617265205269736b79646e616d656654617070797365696d6167657835697066733a2f2f516d57646648616746414e384b4533376750676643374a48593970416e6e6d4e735137563831387162716354337a6757656273697465781868747470733a2f2f7777772e746170746f6f6c732e696f2f6158781c68747470733a2f2f747769747465722e636f6d2f546170546f6f6c73667469636b6572655441505059"

                    decoded =
                        transactionCbor
                            |> Hex.toBytesUnchecked
                            |> Bytes.fromBytes
                            |> Transaction.deserialize
                            |> Maybe.andThen .auxiliaryData
                            |> Maybe.andThen
                                (\auxiliaryData ->
                                    auxiliaryData.labels
                                        |> List.filter (\( label, _ ) -> label == Cip25.label)
                                        |> List.head
                                        |> Maybe.map Tuple.second
                                )
                            |> Maybe.andThen
                                (\metadatum ->
                                    metadatum
                                        |> Metadatum.toCbor
                                        |> E.encode
                                        |> D.decode Cip25.fromCbor
                                )
                            |> Maybe.andThen (Cip25.getAssetMetadata tappyPolicyId tappyAssetName)
                in
                decoded
                    |> Expect.equal (Just tappyAssetMetadata)
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
                                , image = "ipfs://QmetLQaD5vvrsXC8xNhPsAfU3Rx9z1rAPZZLivoPqVwSrq"
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
