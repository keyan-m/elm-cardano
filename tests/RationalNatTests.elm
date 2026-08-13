module RationalNatTests exposing (suite)

import Expect
import Natural
import RationalNat
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "RationalNat.ceiling"
        [ test "zero" <|
            \_ ->
                RationalNat.ceiling RationalNat.zero
                    |> Expect.equal (Just Natural.zero)
        , test "exact integer" <|
            \_ ->
                RationalNat.ceiling
                    { num = Natural.fromSafeInt 6
                    , denom = Natural.fromSafeInt 3
                    }
                    |> Expect.equal (Just (Natural.fromSafeInt 2))
        , test "fractional value" <|
            \_ ->
                RationalNat.ceiling
                    { num = Natural.fromSafeInt 7
                    , denom = Natural.fromSafeInt 3
                    }
                    |> Expect.equal (Just (Natural.fromSafeInt 3))
        , test "zero denominator" <|
            \_ ->
                RationalNat.ceiling
                    { num = Natural.one
                    , denom = Natural.zero
                    }
                    |> Expect.equal Nothing
        ]
