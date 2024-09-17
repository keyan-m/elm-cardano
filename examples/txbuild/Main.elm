module Main exposing (main)

import Browser
import Bytes.Comparable as Bytes
import Cardano
import Cardano.Transaction as Transaction
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo
import Html exposing (Html, div, text)


main : Program () () ()
main =
    Browser.element
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        }


example ex =
    case ex () of
        Err error ->
            Debug.toString error

        Ok tx ->
            Cardano.prettyTx tx


snekTxBytes =
    "84a80081825820507042edb4871c8d5879f0e609853211e8def808e5ac2e5ed0fa16eedbef0a3c0701858258390122aea2da15e494e01767145d48bda16b6d437f1c449823a044193daf299a82ef56311aa10adf04c0072d4870eb9f4d5ff315132434841b741a017d7840825839018282d03d1523518a2683047b93c0949312599407362c14ed57c915ae279e975cb8ea96c363b213e3618fa4b450d8ae4c2bd55be7d046e404821a00126a10a1581cdc3527a71dfb0ce0702c08b245f821c6164581857438fc519614a3d9a152536e656b2e66756e205a4544202d204e465401a300583931905ab869961b094f1b8197278cfe15b45cbe49fa8f32c6b014f85a2db2f6abf60ccde92eae1a2f4fdf65f2eaf6208d872c6f0e597cc10b0701821a093d1cc0a2581c63f947b8d9535bc4e4ce6919e3dc056547e8d30ada12f29aa5f826b8a15820707c0e34ce1d33d0c64d7428deaec14b9f80bd85435c4b3beb2278de49509ad301581c0ce8b488a6cabcc22068ef7f81298ea6d86e15e3d2d0fd14dee46119a14b5a656420546865204361741a35a7e31b028201d81858e9d87989d87982581c63f947b8d9535bc4e4ce6919e3dc056547e8d30ada12f29aa5f826b85820707c0e34ce1d33d0c64d7428deaec14b9f80bd85435c4b3beb2278de49509ad3d879824040d87982581c0ce8b488a6cabcc22068ef7f81298ea6d86e15e3d2d0fd14dee461194b5a656420546865204361741b0000001166b125ce1a001373c2581cedbf33f5d6e083970648e39175c49ec1c093df76b6e6a0f1473e47761b000000028f200558581c8807fbe6e36b1c35ad6f36f0993e2fc67ab6f2db06041cfa3a53c04a581c554daf3e27e50c8c779713be6ba70f95c35f23f0552a28e5a7a34014825839018282d03d1523518a2683047b93c0949312599407362c14ed57c915ae279e975cb8ea96c363b213e3618fa4b450d8ae4c2bd55be7d046e404821a001215e2a1581c0ce8b488a6cabcc22068ef7f81298ea6d86e15e3d2d0fd14dee46119a14b5a656420546865204361741a05f2e6e5825839018282d03d1523518a2683047b93c0949312599407362c14ed57c915ae279e975cb8ea96c363b213e3618fa4b450d8ae4c2bd55be7d046e4041a38c9b8c6021a001821590758207c89eb12f1a2ef50496de11898cf84f12b15c2677f9613df9b27c7dd081bad9709a3581c63f947b8d9535bc4e4ce6919e3dc056547e8d30ada12f29aa5f826b8a15820707c0e34ce1d33d0c64d7428deaec14b9f80bd85435c4b3beb2278de49509ad301581cdc3527a71dfb0ce0702c08b245f821c6164581857438fc519614a3d9a152536e656b2e66756e205a4544202d204e465401581c0ce8b488a6cabcc22068ef7f81298ea6d86e15e3d2d0fd14dee46119a14b5a656420546865204361741a3b9aca000b58206fa005dddf2a79a86c5785532c30852e203fd09040da54d653a807aac91d3eb10d8182582070ac24ee8414b3cbf2c75f7a739ae31e62f4ed6cb52a8209f2e68c500699c3af000e81581cc145013caa2b9c60ac925ed2d654c1c50ff02eff57c6a55ef2aa779aa3008282582069b12e7566b15126b3f7d19c835cbd22ed0bbab99d3f2a5ac05c71f78067d2d85840df68f1e478801137c3130b58c7ee1a5b91713ff33f7fa759f8ca3414d8c9d7cd91cd70343df5989f0c6b55ae7dc617e073d3114406d907070677265748cde30f825820ee17a51ff7fddf9cf68427801cdf68fab5b9c328a0c32a1b66e32c400fe480255840906a2fd52f62e898caf06ea2e1738d079e1328afb6f44014bba6b838c33fb8f67ebc1704250e4ff146f7acba64f84f1b4142ef09b6fdbc32c27c1ae78a384605058384010001821a0004944f1a092aed09840101d87982d879815820507042edb4871c8d5879f0e609853211e8def808e5ac2e5ed0fa16eedbef0a3c07821a0004944f1a092aed0984010200821a009896801b0000000218711a00068359017d59017a01000032323232323232323232322253330083232325332233300d00200114a0666010444a666018002294054ccc038c008c04400452889980180118080009191919baf374e60240046e9cc0480053012bd8799fd8799f5820507042edb4871c8d5879f0e609853211e8def808e5ac2e5ed0fa16eedbef0a3cff07ff00300f30100010011323370e64646644666601600490001199980600124000eb4dd58008029bae3011001375c602260200026022002664466e9520003300d37520046601a6ea40052f5c0646464a66601e66e1d20000021375c60240022c6026004601c0026ea8c8c040c03c004c04001522010b5a656420546865204361740048202a35ae41cdd598071918071807180700098068011bac300d001300d001300b300c0011498588c008dd480091111980291299980400088028a99980519baf300b300d00100613004300f300d00113002300c0010012323002233002002001230022330020020015573eae815cd2ab9d5744ae848c008dd5000aab9e0159023f59023c01000032323232323232323222325333006323232323232323232323253330113370e90001808004099191919299980c180d80109919299980b99b88480000044c8c94ccc064cdc3a40006030002264646464a66603aa66603a004200229405288a503370e00c900119b8f006001372400264660026eb8c00cc060c00cc06005ccc004dd9980f9810180c00b9bb34c0101010022337140040022c646600200201c44a66603a002298103d87a800013232533301c3375e600a6034004032266e952000330200024bd700998020020009810801180f8009180e8008a51375a60300046eb8c05800458c064004c8c8c94ccc058cdc3a4004002297adef6c6013756603660280046028002646600200200444a6660320022980103d87a8000132323232533301a3371e010004266e9520003301e374c00297ae0133006006003375660360066eb8c064008c074008c06c004c8cc004004010894ccc06000452f5bded8c0264646464a66603266e3d22100002100313301d337606ea4008dd3000998030030019bab301a003375c6030004603800460340026eb8c05c004c03c02058dd5980a800980a800980a000980980098090011bac30100013008003300e001300e002300c001300400214984d958c94ccc018cdc3a4000002264646464a66601a60200042649319299980599b87480000044c8c94ccc040c04c00852616375c602200260120082c60120062c6eb4c038004c038008c030004c01000c58c0100088c014dd5000918019baa0015734aae7555cf2ab9f5740ae855d10159018059017d01000032323232323232323232322253330083232325332233300d00200114a0666010444a666018002294054ccc038c008c04400452889980180118080009199119baf374e60240046e9cc048004c03cc04000530012bd8799fd8799f5820507042edb4871c8d5879f0e609853211e8def808e5ac2e5ed0fa16eedbef0a3cff07ff000011323370e64646644666601600490001199980600124000eb4dd58008029bae3011001375c602260200026022002664466e9520003300d37520046601a6ea40052f5c0646464a66601e66e1d20000021375c60240022c6026004601c0026ea8c8c040c03c004c04001522112536e656b2e66756e205a4544202d204e46540048008dd598071918071807180700098068011bac300d001300d001300b300c0011498588c008dd480091111980291299980400088028a99980519baf300b300d00100613004300f300d00113002300c0010012323002233002002001230022330020020015573eae815cd2ab9d5744ae848c008dd5000aab9e01f5a11902d1a178383063653862343838613663616263633232303638656637663831323938656136643836653135653364326430666431346465653436313139a16b5a65642054686520436174a4646e616d656b5a656420546865204361746b6465736372697074696f6e857840416674657220776f726b696e672066726f6d206e696e6520746f206669766520666f7220746865206d616a6f72697479206f6620686973206c6966652c205a65784064206465636964656420746f206265636f6d65206120747261646f6f722e2048697320636172656572206469646e74206c617374206c6f6e672c206573706563784069616c6c7920616674657220686520636f6e76696e6365642068697320626f737320746f2062757920425443206174204154482e20427574205a65642064696478406e7420676976652075702068652063726561746564205a454420746f2070726f766520746f2068697320626f737320746861742063727970746f2069732077687465726520746865206d6f6e65792069732061742e65696d6167657835697066733a2f2f516d526247344d7844686733674d3531426d476f7959734135733650394844785176504a783666366e6471634c59667469636b6572635a4544"
        |> Bytes.fromStringUnchecked


snekTx =
    snekTxBytes
        |> Transaction.deserialize
        |> Maybe.withDefault Transaction.new


oldTx1 =
    "84a400818258200745af3bd39987a1a89652f48ee0abc7c682b780dfd0b3ec8fd9d732b8df891200018d8258390154334ad2b08fdfc4a4c835cdffb8c18150eb2f4825f4141de0d12ae2418ec0356e42e7a831d8a576f67be9d64751f0ac498a45e9d825974d1a1338f5a082584c82d818584283581c9aed52a725c2e96399851f647bc827b7d548891c8f444015afe64519a101581e581cbeab7b5c6b45ef49915760a887eb2af984a147b81d017ef68ebde445001aa6c475651a29bfd38182583901ac7ed2ed3672dcecb577ade8f70436ba8ff8d2911346ed60c51585624fcb572c1db26e847dab1fee7fc22700ac7de3c23be282cb5275b3211a008c618082583901943b89c038b5695514f7a2fa0c676a07a75398a979bd65518aa3745b0801b63258b7668631d9f0aa89a2b9a75a672d16713338b7590366491b0000000a79d670848258390158d62097ec016df81bf1340ef89d32d213f85c513a8dc5029e684a780d8f23617f77a8e5a0dbbd77c0fe148cdce26e63992a512bc6ec5e811a008f189982583901c4bb64f7c67acd03ad1b1282d3ceedf1e8b25540155645dbcf2fc2906d631a286e34b4d336a1f590503f708ef5ace435b8c113329c0bcff51a1377190c8258390180e86ac3e837a50cfdea20bd71de3a1e7a407b8f9c744e460ec06bf33e4b09d8ba256389c68c7ddc9c1ffa9d8f730c83d26169778f5322f51a008c6180825839018282d03d1523518a2683047b93c0949312599407362c14ed57c915ae279e975cb8ea96c363b213e3618fa4b450d8ae4c2bd55be7d046e4041a43c0ef1182583901469a559105b1f4c498cdec4df6ba5a4ee3ac1f66a72ac1397f70dd1ca4d0d2ff325cb761450ab8429e2df0915dce1932d4ba520c6ae09c291a02eebb808258390105739443009e4a4cc1c7d523359589321c8c3e3aeeea61c635dbdf37fa196b0ad0a725a2c9fdab562d0b68a0d38ffa0d9aa0c4da7ceb37b11a2c7d09f0825839011a4655f2e8fe52a7c7739aa8fcd060b7ffcc0af91c709e1578f81e62c055772145a7aa46bb7dc0ee9a3a3dc3f93e09605d79564641a21f701a1dc1300082583901a3d581ca73126b1eb3c66cc6cc39ec31687fed065cf3a66489e2ff4c28f118d02277461441ac46a587e4864bb937eda4972135678008c7911a19b2088082581d61bd251ebb0668572b76ec7484d0c07dd9c3ee0a29cb4a4536a02f108f1a2e401dc1021a00033770031a07fc862ca100818258200d5b7ec7968441393f7fa664f4177938629e778f8b408d43a94990736037d0fb5840a8bf050ec971d1d1857c1ded48601a710e2db90108926c8a77bc88c9bb68f747f0154f8b5a437627acc3c447b995423565630cc654537f90851517f91531bf04f5f6"
        |> Bytes.fromStringUnchecked
        |> Transaction.deserialize
        |> Maybe.withDefault Transaction.new


oldOutput1 =
    List.drop 7 oldTx1.body.outputs
        |> List.head


oldTx2 =
    "84a50081825820c6a75b5069c9b3c7de5dfc3d47965048471a34bc199e51d14bec570f0d6d85f300018682583901c145013caa2b9c60ac925ed2d654c1c50ff02eff57c6a55ef2aa779abc0f8b84ce3535374d22b6a46ad7eb6c020fbfd25fc96fb97d17ae821a004c4b4082583901719bee424a97b58b3dca88fe5da6feac6494aa7226f975f3506c5b257846f6bb07f5b2825885e4502679e699b4e60a0c4609a46bc35454cd1a02d25bb482583901719bee424a97b58b3dca88fe5da6feac6494aa7226f975f3506c5b257846f6bb07f5b2825885e4502679e699b4e60a0c4609a46bc35454cd1a016a8d1c82583901719bee424a97b58b3dca88fe5da6feac6494aa7226f975f3506c5b257846f6bb07f5b2825885e4502679e699b4e60a0c4609a46bc35454cd1a00b5468f82583901719bee424a97b58b3dca88fe5da6feac6494aa7226f975f3506c5b257846f6bb07f5b2825885e4502679e699b4e60a0c4609a46bc35454cd1a00b5468e82583901719bee424a97b58b3dca88fe5da6feac6494aa7226f975f3506c5b257846f6bb07f5b2825885e4502679e699b4e60a0c4609a46bc35454cd1a004c4b40021a0002be85031a0782f3da0800a10081825820834a084bba4b79e8c79d0bbdc8611c108664d8a9a6c2c1b9e65c54c4927fa7d0584069375e9abbb978f2d605bf8d64561e81f67a744b453a764caf650ddc8c574d42c64b5be4c988cdee244d28eee6888f7d17da47912c6a001b908bb01670299303f5f6"
        |> Bytes.fromStringUnchecked
        |> Transaction.deserialize
        |> Maybe.withDefault Transaction.new


oldOutput2 =
    List.head oldTx2.body.outputs


snekLocalUtxos =
    Maybe.withDefault Utxo.emptyRefDict <|
        Maybe.map2
            (\o1 o2 ->
                Utxo.refDictFromList
                    [ ( { transactionId = Bytes.fromStringUnchecked "507042edb4871c8d5879f0e609853211e8def808e5ac2e5ed0fa16eedbef0a3c", outputIndex = 7 }, o1 )
                    , ( { transactionId = Bytes.fromStringUnchecked "70ac24ee8414b3cbf2c75f7a739ae31e62f4ed6cb52a8209f2e68c500699c3af", outputIndex = 0 }, o2 )
                    ]
            )
            oldOutput1
            oldOutput2


snekDeclaredCosts =
    snekTx.witnessSet.redeemer
        |> Maybe.withDefault []
        |> List.map .exUnits


snekActualCostsRaw =
    Uplc.evalScriptsCostsRaw
        { budget = Uplc.conwayDefaultBudget
        , slotConfig = Uplc.slotConfigMainnet
        , costModels = Uplc.conwayDefaultCostModels
        }
        snekLocalUtxos
        snekTxBytes
        |> Result.withDefault []
        |> List.map .exUnits


snekActualCosts =
    Uplc.evalScriptsCosts
        { budget = Uplc.conwayDefaultBudget
        , slotConfig = Uplc.slotConfigMainnet
        , costModels = Uplc.conwayDefaultCostModels
        }
        snekLocalUtxos
        snekTx
        |> Debug.log "eval result"
        |> Result.withDefault []
        |> List.map .exUnits



-- VIEW


view : () -> Html ()
view _ =
    div []
        [ div [] [ text "Example transaction 1: send 1 ada from me to you." ]
        , Html.pre [] [ text <| example Cardano.example1 ]
        , div [] [ text "Example transaction 2: mint 1 dog & burn 1 cat." ]
        , Html.pre [] [ text <| example Cardano.example2 ]
        , div [] [ text "Example transaction 3: spend 2 ada from a plutus script with 4 ada." ]
        , div [] [ text "Spent UTxO index is passed as argument in the redeemer." ]
        , Html.pre [] [ text <| example Cardano.example3 ]

        -- , div [] [ text "SnekDotFun Tx:" ]
        -- , Html.pre [] [ text <| Cardano.prettyTx snekTx ]
        , div [] [ text "SnekDotFun Tx declared execution costs:" ]
        , div [] <| List.map viewCost snekDeclaredCosts
        , div [] [ text "SnekDotFun Tx actual execution costs (evaluated with uplc_wasm on the raw Tx bytes):" ]
        , div [] <| List.map viewCost snekActualCostsRaw
        , div [] [ text "SnekDotFun Tx actual execution costs (evaluated with uplc_wasm on the decoded Tx):" ]
        , div [] <| List.map viewCost snekActualCosts
        ]


viewCost cost =
    Html.pre [] [ text <| Debug.toString cost ]