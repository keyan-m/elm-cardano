module Cardano.TransactionFeeTests exposing (suite)

import Cardano.Data as Data
import Cardano.Redeemer as Redeemer
import Cardano.Transaction as Transaction
import Expect
import Natural
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Transaction fees"
        [ test "linear fee parameters use unbounded naturals" <|
            \_ ->
                let
                    largeBaseFee =
                        Natural.fromSafeString "100000000000000000000"
                in
                Transaction.computeTxSizeFee
                    { baseFee = largeBaseFee
                    , feePerByte = Natural.zero
                    }
                    Transaction.new
                    |> Expect.equal largeBaseFee
        , test "execution price applies one ceiling after combining memory and CPU" <|
            \_ ->
                let
                    prices =
                        { memPrice = { numerator = 1, denominator = 2 }
                        , stepPrice = { numerator = 1, denominator = 2 }
                        }

                    newTx =
                        Transaction.new

                    newWitnessSet =
                        Transaction.newWitnessSet

                    tx =
                        { newTx
                            | witnessSet =
                                { newWitnessSet
                                    | redeemer =
                                        Just
                                            [ { tag = Redeemer.Spend
                                              , index = 0
                                              , data = Data.Constr Natural.zero []
                                              , exUnits = { mem = 1, steps = 1 }
                                              }
                                            ]
                                }
                        }
                in
                Expect.all
                    [ \_ ->
                        Redeemer.feeCost prices { mem = 1, steps = 1 }
                            |> Expect.equal Natural.one
                    , \_ ->
                        Transaction.computeScriptExecFee prices tx
                            |> Expect.equal Natural.one
                    ]
                    ()
        , test "reference-script base price is rational with a final floor" <|
            \_ ->
                Transaction.computeRefScriptFee
                    { minFeeRefScriptCostPerByte = { numerator = 1, denominator = 2 }
                    , multiplier = { numerator = 1, denominator = 1 }
                    , sizeIncrement = 25600
                    }
                    3
                    |> Expect.equal Natural.one
        , test "default reference-script price is fifteen lovelace per byte" <|
            \_ ->
                Transaction.defaultTxFeeParams.refScriptFeeParams.minFeeRefScriptCostPerByte
                    |> Expect.equal { numerator = 15, denominator = 1 }
        ]
