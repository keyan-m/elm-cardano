module Cardano.CoinSelectionTests exposing (..)

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (Address, NetworkId(..))
import Cardano.CoinSelection as CoinSelection exposing (Error(..), inOrderedList, largestFirst)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, fromLovelace)
import Cardano.Value as Value exposing (Value, onlyLovelace)
import Dict.Any
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Extra
import Natural as N
import Result.Extra as Result
import Test exposing (Test, describe, expectDistribution, fuzzWith, test)
import Test.Distribution as Distribution


suite : Test
suite =
    describe "CoinSelection"
        -- Ada only tests
        [ describe "largestFirst ada only"
            [ test "basic scenario" <| largestBasicScenarioTest
            , test "no utxos" <| noOutputsTest largestFirst
            , test "insufficient funds" <| largestInsufficientFundsTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueTest largestFirst
            , test "target zero, already selected output" <| targetZeroAlreadySelectedOutputTest largestFirst
            , fuzzCoinSelection "coverage of payments" <| propCoverageOfPayment largestFirst
            , fuzzCoinSelection "correctness of change" <| propCorrectnessOfChange largestFirst
            ]
        , describe "inOrderedList ada only"
            [ test "basic scenario" <| orderedBasicScenarioTest
            , test "no utxos" <| noOutputsTest inOrderedList
            , test "insufficient funds" <| orderedInsufficientFundsTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueTest inOrderedList
            , test "target zero, already selected output" <| targetZeroAlreadySelectedOutputTest inOrderedList
            , fuzzCoinSelection "coverage of payments" <| propCoverageOfPayment inOrderedList
            , fuzzCoinSelection "correctness of change" <| propCorrectnessOfChange inOrderedList
            ]

        -- MultiAsset tests
        , describe "largestFirst MultiAsset"
            [ test "basic scenario" <| largestBasicScenarioMultiAssetTest
            , test "no utxos" <| noOutputsMultiAssetTest largestFirst
            , test "insufficient funds" <| largestInsufficientFundsMultiAssetTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueMultiAssetTest largestFirst
            , test "target zero, already selected output" <| targetZeroAlreadySelectedOutputMultiAssetTest largestFirst
            ]
        , describe "inOrderedList MultiAsset"
            [ test "basic scenario" <| orderedBasicScenarioMultiAssetTest
            , test "no utxos" <| noOutputsMultiAssetTest inOrderedList
            , test "insufficient funds" <| orderedInsufficientFundsMultiAssetTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueMultiAssetTest inOrderedList
            , test "target zero, already selected output" <| targetZeroAlreadySelectedOutputMultiAssetTest inOrderedList
            ]
        , describe "per-address minimum Ada"
            [ test "uses the supplied lovelace-per-UTxO-byte parameter" perAddressMinAdaTest
            ]
        , describe "collateral"
            [ test "omits an exact-zero return" collateralZeroReturnTest
            , test "continues selecting until a custom minimum-Ada return is valid" collateralCustomMinAdaTest
            , test "honors the custom maximum collateral input count" collateralMaxInputCountTest
            , test "prioritizes a sufficient fallback before reaching the input limit" collateralFallbackPriorityTest
            , test "preserves native assets in the collateral return" collateralPreservesAssetsTest
            ]
        ]



-- Ada only


largestBasicScenarioTest : a -> Expectation
largestBasicScenarioTest _ =
    let
        context =
            { availableUtxos =
                [ output "1" 50
                , output "2" 30
                , output "3" 20
                ]
            , alreadySelectedUtxos = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 30
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedUtxos = [ output "1" 50 ]
                , change = Just <| onlyLovelace (N.fromSafeInt 20)
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult


orderedBasicScenarioTest : a -> Expectation
orderedBasicScenarioTest _ =
    let
        context =
            { availableUtxos =
                [ output "1" 20
                , output "2" 30
                , output "3" 50
                ]
            , alreadySelectedUtxos = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 30
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedUtxos = [ output "2" 30, output "1" 20 ]
                , change = Just <| onlyLovelace (N.fromSafeInt 20)
                }
    in
    inOrderedList maxInputCount context
        |> Expect.equal expectedResult


noOutputsTest : CoinSelection.Algorithm -> a -> Expectation
noOutputsTest selectionAlgo _ =
    let
        context =
            { availableUtxos = []
            , alreadySelectedUtxos = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 30
            }

        maxInputCount =
            5
    in
    selectionAlgo maxInputCount context
        |> Expect.equal (Err <| UTxOBalanceInsufficient { selectedUtxos = [], missingValue = context.targetAmount })


largestInsufficientFundsTest : a -> Expectation
largestInsufficientFundsTest _ =
    let
        availableOutputs =
            [ output "1" 5
            , output "2" 10
            ]

        context =
            { availableUtxos = availableOutputs
            , alreadySelectedUtxos = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 30
            }
    in
    largestFirst 5 context
        |> Expect.equal
            (Err <|
                UTxOBalanceInsufficient
                    { selectedUtxos = availableOutputs
                    , missingValue = onlyLovelace <| N.fromSafeInt 15
                    }
            )


orderedInsufficientFundsTest : a -> Expectation
orderedInsufficientFundsTest _ =
    let
        availableOutputs =
            [ output "1" 5
            , output "2" 10
            ]

        context =
            { availableUtxos = availableOutputs
            , alreadySelectedUtxos = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 30
            }
    in
    inOrderedList 5 context
        |> Expect.equal
            (Err <|
                UTxOBalanceInsufficient
                    { selectedUtxos = List.reverse availableOutputs
                    , missingValue = onlyLovelace <| N.fromSafeInt 15
                    }
            )


singleUtxoSingleOutputEqualValueTest : CoinSelection.Algorithm -> a -> Expectation
singleUtxoSingleOutputEqualValueTest algorithm _ =
    let
        context =
            { availableUtxos = [ output "1" 10 ]
            , alreadySelectedUtxos = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 10
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedUtxos = context.availableUtxos
                , change = Nothing
                }
    in
    algorithm maxInputCount context
        |> Expect.equal expectedResult


targetZeroAlreadySelectedOutputTest : CoinSelection.Algorithm -> a -> Expectation
targetZeroAlreadySelectedOutputTest algorithm _ =
    let
        context =
            { availableUtxos = []
            , alreadySelectedUtxos = [ output "1" 1 ]
            , targetAmount = Value.zero
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedUtxos = [ output "1" 1 ]
                , change = Just <| onlyLovelace (N.fromSafeInt 1)
                }
    in
    algorithm maxInputCount context
        |> Expect.equal expectedResult


perAddressMinAdaTest : () -> Expectation
perAddressMinAdaTest _ =
    let
        addr =
            selectionAddress

        owedValue =
            onlyLovelace (N.fromSafeInt 196)

        config _ =
            { selectionAlgo = inOrderedList
            , normalizationAlgo = \{ target, owed } -> { normalizedTarget = target, normalizedOwed = owed }
            , changeAlgo = \value -> [ value ]
            }

        contexts =
            Address.dictFromList
                [ ( addr
                  , { availableUtxos = []
                    , alreadySelectedUtxos = []
                    , targetValue = Value.zero
                    , alreadyOwed = owedValue
                    }
                  )
                ]
    in
    case
        ( CoinSelection.perAddressWith N.one config contexts
        , CoinSelection.perAddressWith N.two config contexts
        )
    of
        ( Ok lowCostSelections, Err _ ) ->
            case Dict.Any.get addr lowCostSelections of
                Just { changeOutputs } ->
                    case changeOutputs of
                        [ changeOutput ] ->
                            Expect.equal owedValue changeOutput.amount

                        _ ->
                            Expect.fail "expected one change output at the supplied low minimum-Ada price"

                Nothing ->
                    Expect.fail "expected a selection for the requested address"

        _ ->
            Expect.fail "expected only the lower minimum-Ada price to accept the output"


collateralZeroReturnTest : () -> Expectation
collateralZeroReturnTest _ =
    let
        target =
            N.fromSafeInt 2000000

        context =
            collateralContext
                [ collateralOutput "exact" (onlyLovelace target) ]
                target
    in
    case CoinSelection.collateralWith defaultCollateralConfig context of
        Ok selection ->
            Expect.equal ( 1, Nothing ) ( List.length selection.selectedUtxos, selection.change )

        Err err ->
            Expect.fail ("unexpected collateral selection error: " ++ Debug.toString err)


collateralCustomMinAdaTest : () -> Expectation
collateralCustomMinAdaTest _ =
    let
        target =
            N.fromSafeInt 1000000

        adaPerUtxoByte =
            N.fromSafeInt 40000

        minimumReturn =
            minimumAdaOnlyReturn adaPerUtxoByte

        candidateAmount =
            N.add target (N.sub minimumReturn N.one)

        context =
            collateralContext
                [ collateralOutput "first" (onlyLovelace candidateAmount)
                , collateralOutput "second" (onlyLovelace candidateAmount)
                ]
                target

        config =
            { adaPerUtxoByte = adaPerUtxoByte
            , maxInputCount = 2
            }
    in
    case CoinSelection.collateralWith config context of
        Ok selection ->
            let
                expectedChange =
                    N.sub (N.mul N.two candidateAmount) target
                        |> onlyLovelace
            in
            Expect.equal ( 2, Just expectedChange )
                ( List.length selection.selectedUtxos, selection.change )

        Err err ->
            Expect.fail ("unexpected collateral selection error: " ++ Debug.toString err)


collateralMaxInputCountTest : () -> Expectation
collateralMaxInputCountTest _ =
    let
        target =
            N.fromSafeInt 1000000

        adaPerUtxoByte =
            N.fromSafeInt 40000

        candidateAmount =
            N.add target (N.sub (minimumAdaOnlyReturn adaPerUtxoByte) N.one)

        context =
            collateralContext
                [ collateralOutput "first" (onlyLovelace candidateAmount)
                , collateralOutput "second" (onlyLovelace candidateAmount)
                ]
                target

        config =
            { adaPerUtxoByte = adaPerUtxoByte
            , maxInputCount = 1
            }
    in
    CoinSelection.collateralWith config context
        |> Expect.equal (Err MaximumInputCountExceeded)


collateralFallbackPriorityTest : () -> Expectation
collateralFallbackPriorityTest _ =
    let
        target =
            N.fromSafeInt 1000000

        adaPerUtxoByte =
            N.fromSafeInt 40000

        minimumReturn =
            minimumAdaOnlyReturn adaPerUtxoByte

        invalidCandidateAmount =
            N.add target (N.sub minimumReturn N.one)

        validCandidateAmount =
            N.add target minimumReturn

        context =
            collateralContext
                [ collateralOutput "invalid-return" (onlyLovelace invalidCandidateAmount)
                , collateralOutput "valid-return" (onlyLovelace validCandidateAmount)
                ]
                target

        config =
            { adaPerUtxoByte = adaPerUtxoByte
            , maxInputCount = 1
            }
    in
    CoinSelection.collateralWith config context
        |> Expect.equal
            (Ok
                { selectedUtxos = [ collateralOutput "valid-return" (onlyLovelace validCandidateAmount) ]
                , change = Just (onlyLovelace minimumReturn)
                }
            )


collateralPreservesAssetsTest : () -> Expectation
collateralPreservesAssetsTest _ =
    let
        tokenValue =
            Value.onlyToken
                (Bytes.dummy 28 "collateral-policy")
                (Bytes.fromText "asset")
                N.one

        combinedTokens =
            Value.add tokenValue tokenValue

        target =
            Utxo.minAdaWith
                defaultCollateralConfig.adaPerUtxoByte
                (Utxo.simpleOutput selectionAddress combinedTokens)

        candidateValue =
            Value.add (onlyLovelace target) tokenValue

        context =
            collateralContext
                [ collateralOutput "asset-a" candidateValue
                , collateralOutput "asset-b" candidateValue
                ]
                target
    in
    case CoinSelection.collateralWith defaultCollateralConfig context of
        Ok selection ->
            let
                expectedChange =
                    Value.add (onlyLovelace target) combinedTokens
            in
            Expect.equal ( 2, Just expectedChange )
                ( List.length selection.selectedUtxos, selection.change )

        Err err ->
            Expect.fail ("unexpected collateral selection error: " ++ Debug.toString err)


minimumAdaOnlyReturn : N.Natural -> N.Natural
minimumAdaOnlyReturn adaPerUtxoByte =
    Utxo.minAdaWith
        adaPerUtxoByte
        (Utxo.simpleOutput selectionAddress Value.zero)


defaultCollateralConfig : { adaPerUtxoByte : N.Natural, maxInputCount : Int }
defaultCollateralConfig =
    { adaPerUtxoByte = N.fromSafeInt 4310
    , maxInputCount = 3
    }


collateralContext : List ( OutputReference, Output ) -> N.Natural -> CoinSelection.CollateralContext
collateralContext availableUtxos targetAmount =
    { availableUtxos = availableUtxos
    , allowedAddresses = Address.dictFromList [ ( selectionAddress, () ) ]
    , targetAmount = targetAmount
    }


collateralOutput : String -> Value -> ( OutputReference, Output )
collateralOutput label value =
    ( OutputReference (Bytes.dummy 32 label) 0
    , Utxo.simpleOutput selectionAddress value
    )


selectionAddress : Address
selectionAddress =
    Address.enterprise Testnet (Bytes.dummy 28 "selection")



-- Fixtures


output : String -> Int -> ( OutputReference, Output )
output addrSuffix amount =
    ( OutputReference (Bytes.fromHexUnchecked "addrSuffix") 0
    , fromLovelace (address addrSuffix) (N.fromSafeInt amount)
    )


address : String -> Address
address suffix =
    Bytes.fromHexUnchecked ("addr" ++ suffix)
        |> Address.enterprise Testnet



-- Fuzzer


fuzzCoinSelection : String -> (Int -> CoinSelection.Context -> Expectation) -> Test
fuzzCoinSelection title prop =
    let
        maxInputCount =
            5
    in
    fuzzWith
        { runs = 100
        , distribution = contextDistribution maxInputCount
        }
        (contextFuzzer maxInputCount)
        title
        (prop maxInputCount)


contextFuzzer : Int -> Fuzzer CoinSelection.Context
contextFuzzer maxInputCount =
    let
        outputFuzzer =
            Fuzz.map2 output
                (Fuzz.map String.fromInt Fuzz.int)
                (Fuzz.intAtLeast 1)
    in
    Fuzz.map3 CoinSelection.Context
        (Fuzz.frequency
            [ ( 1, Fuzz.constant [] )
            , ( 9, Fuzz.listOfLengthBetween 1 (maxInputCount + 1) outputFuzzer )
            ]
        )
        (Fuzz.frequency
            [ ( 1, Fuzz.listOfLengthBetween 0 maxInputCount outputFuzzer )
            , ( 9, Fuzz.constant [] )
            ]
        )
        (Fuzz.map onlyLovelace Fuzz.Extra.natural)


contextDistribution : Int -> Test.Distribution CoinSelection.Context
contextDistribution maxInputCount =
    expectDistribution
        [ ( Distribution.atLeast 70
          , "success"
          , \ctx -> largestFirst maxInputCount ctx |> Result.isOk
          )
        , ( Distribution.atLeast 80
          , "no already selected outputs"
          , \ctx -> ctx.alreadySelectedUtxos |> List.isEmpty
          )
        , ( Distribution.atLeast 5
          , "already selected outputs"
          , \ctx -> ctx.alreadySelectedUtxos |> List.isEmpty |> not
          )
        ]



-- Properties


propCoverageOfPayment : CoinSelection.Algorithm -> Int -> CoinSelection.Context -> Expectation
propCoverageOfPayment algorithm maxInputCount context =
    case algorithm maxInputCount context of
        Err _ ->
            Expect.pass

        Ok { selectedUtxos } ->
            Value.sum (List.map (Tuple.second >> .amount) selectedUtxos)
                -- |> Expect.atLeast context.targetAmount
                |> Value.atLeast context.targetAmount
                |> Expect.equal True


propCorrectnessOfChange : CoinSelection.Algorithm -> Int -> CoinSelection.Context -> Expectation
propCorrectnessOfChange algorithm maxInputCount context =
    case algorithm maxInputCount context of
        Err _ ->
            Expect.pass

        Ok { selectedUtxos, change } ->
            let
                changeAmount =
                    Maybe.withDefault Value.zero change
            in
            Value.sum (List.map (Tuple.second >> .amount) selectedUtxos)
                |> Expect.equal (Value.add changeAmount context.targetAmount)



-- MultiAsset


largestBasicScenarioMultiAssetTest : a -> Expectation
largestBasicScenarioMultiAssetTest _ =
    let
        context =
            { availableUtxos =
                [ asset "1" "policy" "name" 30
                , asset "2" "policy" "name" 20
                , asset "3" "policy" "name" 70
                , asset "4" "policy" "name" 10
                ]
            , alreadySelectedUtxos = []
            , targetAmount = token "policy" "name" 30
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedUtxos = [ asset "3" "policy" "name" 70 ]
                , change = Just (token "policy" "name" 40)
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult


orderedBasicScenarioMultiAssetTest : a -> Expectation
orderedBasicScenarioMultiAssetTest _ =
    let
        context =
            { availableUtxos =
                [ asset "1" "policy" "name" 20
                , asset "2" "policy" "name" 30
                , asset "3" "policy" "name" 70
                , asset "4" "policy" "name" 10
                ]
            , alreadySelectedUtxos = []
            , targetAmount = token "policy" "name" 30
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedUtxos = [ asset "2" "policy" "name" 30, asset "1" "policy" "name" 20 ]
                , change = Just (token "policy" "name" 20)
                }
    in
    inOrderedList maxInputCount context
        |> Expect.equal expectedResult


noOutputsMultiAssetTest : CoinSelection.Algorithm -> a -> Expectation
noOutputsMultiAssetTest algorithm _ =
    let
        context =
            { availableUtxos = []
            , alreadySelectedUtxos = []
            , targetAmount = token "policy" "name" 30
            }

        maxInputCount =
            5
    in
    algorithm maxInputCount context
        |> Expect.equal (Err <| UTxOBalanceInsufficient { selectedUtxos = [], missingValue = context.targetAmount })


largestInsufficientFundsMultiAssetTest : a -> Expectation
largestInsufficientFundsMultiAssetTest _ =
    let
        availableOutputs =
            [ asset "1" "policy" "name" 5
            , asset "2" "policy" "name" 10
            ]

        context =
            { availableUtxos = availableOutputs
            , alreadySelectedUtxos = []
            , targetAmount = token "policy" "name" 30
            }
    in
    largestFirst 5 context
        |> Expect.equal
            (Err <|
                UTxOBalanceInsufficient
                    { selectedUtxos = availableOutputs
                    , missingValue = token "policy" "name" 15
                    }
            )


orderedInsufficientFundsMultiAssetTest : a -> Expectation
orderedInsufficientFundsMultiAssetTest _ =
    let
        availableOutputs =
            [ asset "1" "policy" "name" 5
            , asset "2" "policy" "name" 10
            ]

        context =
            { availableUtxos = availableOutputs
            , alreadySelectedUtxos = []
            , targetAmount = token "policy" "name" 30
            }
    in
    inOrderedList 5 context
        |> Expect.equal
            (Err <|
                UTxOBalanceInsufficient
                    { selectedUtxos = List.reverse availableOutputs
                    , missingValue = token "policy" "name" 15
                    }
            )


singleUtxoSingleOutputEqualValueMultiAssetTest : CoinSelection.Algorithm -> a -> Expectation
singleUtxoSingleOutputEqualValueMultiAssetTest algorithm _ =
    let
        context =
            { availableUtxos = [ asset "1" "policy" "name" 10 ]
            , alreadySelectedUtxos = []
            , targetAmount = token "policy" "name" 10
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedUtxos = context.availableUtxos
                , change = Nothing
                }
    in
    algorithm maxInputCount context
        |> Expect.equal expectedResult


targetZeroAlreadySelectedOutputMultiAssetTest : CoinSelection.Algorithm -> a -> Expectation
targetZeroAlreadySelectedOutputMultiAssetTest algorithm _ =
    let
        context =
            { availableUtxos = []
            , alreadySelectedUtxos = [ asset "1" "policy" "name" 1 ]
            , targetAmount = Value.zero
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedUtxos = [ asset "1" "policy" "name" 1 ]
                , change = Just <| token "policy" "name" 1
                }
    in
    algorithm maxInputCount context
        |> Expect.equal expectedResult



-- Helper functions


asset : String -> String -> String -> Int -> ( OutputReference, Output )
asset addrSuffix policyId name amount =
    ( OutputReference (Bytes.fromHexUnchecked <| "Tx" ++ addrSuffix) 0
    , { address = address addrSuffix
      , amount = token policyId name amount
      , datumOption = Nothing
      , referenceScript = Nothing
      }
    )


token : String -> String -> Int -> Value
token policyId name amount =
    Value.onlyToken (Bytes.fromHexUnchecked policyId) (Bytes.fromHexUnchecked name) (N.fromSafeInt amount)
