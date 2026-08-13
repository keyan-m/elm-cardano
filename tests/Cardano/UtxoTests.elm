module Cardano.UtxoTests exposing (suite)

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (NetworkId(..))
import Cardano.Utxo as Utxo exposing (Output)
import Expect
import Natural as N exposing (Natural)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Utxo minimum Ada"
        [ test "the default delegates to 4310 lovelace per byte" <|
            \_ ->
                Expect.equal
                    (Utxo.minAdaWith (N.fromSafeInt 4310) (outputWith N.zero))
                    (Utxo.minAda (outputWith N.zero))
        , test "converges when the result uses a wider CBOR coin encoding" <|
            \_ ->
                Utxo.minAdaWith N.one (outputWith N.zero)
                    |> Expect.equal (N.fromSafeInt 196)
        , test "converges across the 16-bit CBOR coin boundary" <|
            \_ ->
                Utxo.minAdaWith (N.fromSafeInt 333) (outputWith N.zero)
                    |> Expect.equal (N.fromSafeInt 66267)
        , test "uses Natural arithmetic for large protocol parameters" <|
            \_ ->
                let
                    adaPerUtxoByte =
                        N.fromSafeString "10000000000000000"

                    minimum =
                        Utxo.minAdaWith adaPerUtxoByte (outputWith N.zero)

                    outputAtMinimum =
                        outputWith minimum

                    recomputed =
                        N.mul
                            adaPerUtxoByte
                            (N.fromSafeInt (160 + Utxo.bytesWidth outputAtMinimum))
                in
                Expect.equal recomputed minimum
        , test "checkMinAdaWith accepts the exact minimum and rejects one less" <|
            \_ ->
                let
                    adaPerUtxoByte =
                        N.fromSafeInt 333

                    minimum =
                        Utxo.minAdaWith adaPerUtxoByte (outputWith N.zero)
                in
                case
                    ( Utxo.checkMinAdaWith adaPerUtxoByte (outputWith minimum)
                    , Utxo.checkMinAdaWith adaPerUtxoByte (outputWith (N.sub minimum N.one))
                    )
                of
                    ( Ok _, Err _ ) ->
                        Expect.pass

                    _ ->
                        Expect.fail "expected only the output at the exact minimum to pass"
        ]


outputWith : Natural -> Output
outputWith lovelace =
    Utxo.fromLovelace
        (Address.enterprise Testnet (Bytes.dummy 28 "min-ada"))
        lovelace
