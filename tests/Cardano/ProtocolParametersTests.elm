module Cardano.ProtocolParametersTests exposing (suite)

import Cardano.Gov as Gov
import Cardano.ProtocolParameters as ProtocolParameters exposing (ValidationError(..), defaultProtocolParameters)
import Cardano.Transaction as Transaction
import Cardano.Uplc as Uplc
import Expect
import Natural
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Protocol parameters"
        [ test "the static default validates" <|
            \_ ->
                ProtocolParameters.validate ProtocolParameters.defaultProtocolParameters
                    |> Expect.equal (Ok ProtocolParameters.defaultProtocolParameters)
        , test "the default composes existing fee, governance, and VM constants" <|
            \_ ->
                ProtocolParameters.defaultProtocolParameters
                    |> Expect.all
                        [ \parameters -> Expect.equal Transaction.defaultTxFeeParams.feePerByte parameters.minFeeA
                        , \parameters -> Expect.equal Transaction.defaultTxFeeParams.baseFee parameters.minFeeB
                        , \parameters -> Expect.equal Gov.defaultMaxTxSize parameters.maxTransactionSize
                        , \parameters -> Expect.equal Uplc.conwayDefaultBudget parameters.maxTxExUnits
                        , \parameters -> Expect.equal Uplc.conwayDefaultCostModels parameters.costModels
                        , \parameters -> Expect.equal (Natural.fromSafeInt 4310) parameters.adaPerUtxoByte
                        ]
        , test "rejects a negative unsigned integer field" <|
            \_ ->
                let
                    invalid =
                        { defaultProtocolParameters | maxTransactionSize = -1 }
                in
                ProtocolParameters.validate invalid
                    |> Expect.equal
                        (Err
                            (InvalidIntegerParameter
                                { parameter = "maxTransactionSize"
                                , value = -1
                                }
                            )
                        )
        , test "rejects negative execution units" <|
            \_ ->
                let
                    invalid =
                        { defaultProtocolParameters
                            | maxTxExUnits = { mem = -1, steps = 1 }
                        }
                in
                ProtocolParameters.validate invalid
                    |> Expect.equal
                        (Err
                            (InvalidExecutionUnitsParameter
                                { parameter = "maxTxExUnits"
                                , value = { mem = -1, steps = 1 }
                                }
                            )
                        )
        , test "rejects a zero rational denominator" <|
            \_ ->
                let
                    invalid =
                        { defaultProtocolParameters
                            | minFeeRefScriptCostPerByte = { numerator = 1, denominator = 0 }
                        }
                in
                ProtocolParameters.validate invalid
                    |> Expect.equal
                        (Err
                            (InvalidRationalParameter
                                { parameter = "minFeeRefScriptCostPerByte"
                                , value = { numerator = 1, denominator = 0 }
                                }
                            )
                        )
        , test "rejects a negative non-negative ratio" <|
            \_ ->
                let
                    invalid =
                        { defaultProtocolParameters
                            | executionCosts =
                                { memPrice = { numerator = -1, denominator = 10 }
                                , stepPrice = { numerator = 1, denominator = 10 }
                                }
                        }
                in
                ProtocolParameters.validate invalid
                    |> Expect.equal
                        (Err
                            (InvalidRationalParameter
                                { parameter = "executionCosts.memPrice"
                                , value = { numerator = -1, denominator = 10 }
                                }
                            )
                        )
        , test "rejects a unit interval above one" <|
            \_ ->
                let
                    invalid =
                        { defaultProtocolParameters
                            | expansionRate = { numerator = 11, denominator = 10 }
                        }
                in
                ProtocolParameters.validate invalid
                    |> Expect.equal
                        (Err
                            (InvalidUnitIntervalParameter
                                { parameter = "expansionRate"
                                , value = { numerator = 11, denominator = 10 }
                                }
                            )
                        )
        , test "validates nested governance thresholds" <|
            \_ ->
                let
                    defaults =
                        ProtocolParameters.defaultProtocolParameters

                    poolThresholds =
                        defaults.poolVotingThresholds

                    invalid =
                        { defaults
                            | poolVotingThresholds =
                                { poolThresholds
                                    | committeeNormal = { numerator = 1, denominator = 0 }
                                }
                        }
                in
                ProtocolParameters.validate invalid
                    |> Expect.equal
                        (Err
                            (InvalidUnitIntervalParameter
                                { parameter = "poolVotingThresholds.committeeNormal"
                                , value = { numerator = 1, denominator = 0 }
                                }
                            )
                        )
        ]
