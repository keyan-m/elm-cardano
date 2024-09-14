module Blake2b.Int128 exposing
    ( Int128(..)
    , add
    , and
    , complement
    , mul
    , or
    , rotateRightBy
    , shiftLeftBy
    , shiftRightZfBy
    , toHex
    , toUnsigned
    , xor
    )

import Bitwise
import Bytes exposing (Endianness(..))
import UInt64 exposing (UInt64)


type Int128
    = Int128 UInt64 UInt64


and : Int128 -> Int128 -> Int128
and (Int128 a b) (Int128 p q) =
    Int128 (UInt64.and a p) (UInt64.and b q)


complement : Int128 -> Int128
complement (Int128 a b) =
    Int128
        (UInt64.complement a |> UInt64.shiftRightZfBy 0)
        (UInt64.complement b |> UInt64.shiftRightZfBy 0)


or : Int128 -> Int128 -> Int128
or (Int128 a b) (Int128 p q) =
    Int128 (UInt64.or a p) (UInt64.or b q)


xor : Int128 -> Int128 -> Int128
xor (Int128 a b) (Int128 p q) =
    Int128 (UInt64.xor a p) (UInt64.xor b q)


add : Int128 -> Int128 -> Int128
add (Int128 a b) (Int128 p q) =
    let
        lower =
            UInt64.add b q

        higher =
            UInt64.add a p
    in
    -- check for overflow in the lower bits
    case UInt64.compare lower UInt64.maxValue of
        GT ->
            Int128
                (UInt64.add higher UInt64.one)
                lower

        _ ->
            Int128 higher lower


shiftLeftBy : Int -> Int128 -> Int128
shiftLeftBy n (Int128 higher lower) =
    if n > 64 then
        let
            carry =
                UInt64.shiftLeftBy n lower
        in
        Int128 carry UInt64.zero

    else
        let
            carry =
                UInt64.shiftRightZfBy (64 - n) lower

            newHigher =
                higher
                    |> UInt64.shiftLeftBy n
                    |> UInt64.or carry
        in
        Int128 newHigher (UInt64.shiftLeftBy n lower)


shiftRightZfBy : Int -> Int128 -> Int128
shiftRightZfBy n (Int128 higher lower) =
    if n > 64 then
        Int128 UInt64.zero (UInt64.shiftRightZfBy n higher)

    else
        let
            carry =
                UInt64.shiftLeftBy (64 - n) higher

            newLower =
                lower
                    |> UInt64.shiftRightZfBy n
                    |> UInt64.or carry
                    |> UInt64.shiftRightZfBy 0
        in
        Int128 (UInt64.shiftRightZfBy n higher) newLower


rotateRightBy : Int -> Int128 -> Int128
rotateRightBy n (Int128 higher lower) =
    if n > 64 then
        let
            -- guaranteed m <= 64
            m =
                128 - n

            carry =
                UInt64.shiftRightZfBy (64 - m) lower

            p1 =
                higher
                    |> UInt64.shiftLeftBy m
                    |> UInt64.or carry

            p2 =
                UInt64.shiftLeftBy m lower

            q1 =
                UInt64.zero

            q2 =
                UInt64.shiftRightZfBy n higher
        in
        Int128 (UInt64.or p1 q1) (UInt64.or p2 q2)

    else
        let
            -- guaranteed n <= 64, m > 64
            m =
                128 - n

            p1 =
                UInt64.shiftLeftBy m lower

            p2 =
                UInt64.zero

            carry =
                UInt64.shiftLeftBy (64 - n) higher

            q1 =
                UInt64.shiftRightZfBy n higher

            q2 =
                lower
                    |> UInt64.shiftRightZfBy n
                    |> UInt64.or carry
        in
        Int128 (UInt64.or p1 q1) (UInt64.or p2 q2)


{-| Multiplation of 2 U64 numbers without oveflow wrapping, but rather lifting
it into a U128.

If the arguments can fit in 31 bytes, we'll just use the `mul` function from
`UInt64`. Otherwise, we'll break them down into their underlying components
(16 bits <> 24 bits <> 24 bits) and perform the following algorithm:

Breaking down a U64 integer to (U16, U24, U24) integers is mathematically
equivalent to:

    a =
        2 ^ 48 ⋅ aHigh + 2 ^ 24 ⋅ aMid + aLow

    b =
        2 ^ 48 ⋅ bHigh + 2 ^ 24 ⋅ bMid + bLow

If we perform the multiplication between these two terms we'll get:

    a ⋅ b = 2^96 ⋅ z4 + 2^72 ⋅ z3 + 2^48 ⋅ z2 + 2^24 ⋅ z1 + z0

where:

    -- 32 bits
    z4 =
        aHigh ⋅ bHigh

    -- 41 bits
    z3 =
        aHigh ⋅ bMid + aMid ⋅ bHigh

    -- 49 bits
    z2 =
        aHigh ⋅ bLow + aMid ⋅ bMid + aLow ⋅ bHigh

    -- 49 bits
    z1 =
        aMid ⋅ bLow + aLow ⋅ bMid

    -- 48 bits
    z0 =
        aLow ⋅ bLow

Diagram below depicts this summation with black squares as bits that may not be
zero, and white squares for 0 bits:

    |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |      |

                                                                    □□□□□□□□□□□□□□□□■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
                                            □□□□□□□□□□□□□□□■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■□□□□□□□□□□□□□□□□□□□□□□□□
                    □□□□□□□□□□□□□□□■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□
            □□□□□□□■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□
    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□□
    z4

If we notate `zi = (hi, mi, li)` (16 bits, 24 bits, 24 bits), we'll have:

    bytes [ 0 ,  24]: x0 = l0
    bytes [ 25,  48]: x1 = m0 + l1
    bytes [ 49,  64]: x2 = (0xffff & m1) + (0xffff & l2) + carry over from x1

    bytes [ 65,  88]: x3 = (h1 << 8) + (m1 >> 16) + (l2 >> 16) + ((0xffff & m2) << 8) + ((0xffff & l3) << 8) + carry over from x2
    bytes [ 89, 112]: x4 = (h2 << 8) + (m2 >> 16) + (l3 >> 16) + ((0xffff & m3) << 8) + ((0xffff & l4) << 8) + carry over from x3
    bytes [113, 128]: x5 = (l4 >> 16) + (m4 << 8) + (m3 >> 16) + carry over from x4

Therefore, final U128's components will be:

    let
        lsb =
            UInt64.fromInt24s x2 x1 x0

        msb =
            UInt64.fromInt24s x5 x4 x3
    in
    U128 msb lsb

-}
mul : UInt64 -> UInt64 -> Int128
mul a b =
    case ( UInt64.toInt31 a, UInt64.toInt31 b ) of
        ( Just a0, Just b0 ) ->
            Int128 UInt64.zero (UInt64.fromInt (a0 * b0))

        _ ->
            let
                ( aHigh, aMid, aLow ) =
                    UInt64.toInt24s a

                ( bHigh, bMid, bLow ) =
                    UInt64.toInt24s b

                aL =
                    UInt64.fromInt aLow

                aM =
                    UInt64.fromInt aMid

                aH =
                    UInt64.fromInt aHigh

                bL =
                    UInt64.fromInt bLow

                bM =
                    UInt64.fromInt bMid

                bH =
                    UInt64.fromInt bHigh

                z0 =
                    UInt64.mul aL bL

                z1 =
                    UInt64.add
                        (UInt64.mul aM bL)
                        (UInt64.mul aL bM)

                z2 =
                    UInt64.add
                        (UInt64.mul aH bL)
                        (UInt64.add
                            (UInt64.mul aM bM)
                            (UInt64.mul aL bH)
                        )

                z3 =
                    UInt64.add
                        (UInt64.mul aH bM)
                        (UInt64.mul aM bH)

                z4 =
                    UInt64.mul aH bH

                ( _, m0, l0 ) =
                    UInt64.toInt24s z0

                ( h1, m1, l1 ) =
                    UInt64.toInt24s z1

                ( h2, m2, l2 ) =
                    UInt64.toInt24s z2

                ( _, m3, l3 ) =
                    UInt64.toInt24s z3

                ( _, m4, l4 ) =
                    UInt64.toInt24s z4

                findCarryOver sm mx =
                    if sm > mx then
                        1

                    else
                        0

                x0 =
                    l0

                x1 =
                    m0 + l1

                x2 =
                    Bitwise.and 0xFFFF m1
                        + Bitwise.and 0xFFFF l2
                        + findCarryOver x1 0x00FFFFFF

                x3 =
                    Bitwise.shiftLeftBy 8 h1
                        + Bitwise.shiftRightZfBy 16 m1
                        + Bitwise.shiftRightZfBy 16 l2
                        + Bitwise.shiftLeftBy 8 (Bitwise.and 0xFFFF m2)
                        + Bitwise.shiftLeftBy 8 (Bitwise.and 0xFFFF l3)
                        + findCarryOver x2 0xFFFF

                x4 =
                    Bitwise.shiftLeftBy 8 h2
                        + Bitwise.shiftRightZfBy 16 m2
                        + Bitwise.shiftRightZfBy 16 l3
                        + Bitwise.shiftLeftBy 8 (Bitwise.and 0xFFFF m3)
                        + Bitwise.shiftLeftBy 8 (Bitwise.and 0xFFFF l4)
                        + findCarryOver x3 0x00FFFFFF

                x5 =
                    Bitwise.shiftRightZfBy 16 l4
                        + Bitwise.shiftLeftBy 8 m4
                        + Bitwise.shiftRightZfBy 16 m3
                        + findCarryOver x4 0x00FFFFFF

                lsb =
                    UInt64.fromInt24s x2 x1 x0

                msb =
                    UInt64.fromInt24s x5 x4 x3
            in
            Int128
                msb
                lsb


toUnsigned : Int128 -> Int128
toUnsigned (Int128 a b) =
    Int128 (UInt64.shiftRightZfBy 0 a) (UInt64.shiftRightZfBy 0 b)


toHex : Int128 -> String
toHex (Int128 higher lower) =
    let
        high =
            higher
                |> UInt64.toHexString
                |> String.toLower

        low =
            lower
                |> UInt64.toHexString
                |> String.toLower
    in
    high ++ low
