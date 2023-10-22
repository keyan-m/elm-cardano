module ElmCardano.Transaction exposing
    ( Transaction
    , TransactionBody, WitnessSet
    , Value(..), MintedValue, PolicyId, adaAssetName
    , Address, Credential(..), StakeCredential(..)
    , Input, OutputReference, Output(..)
    , ScriptContext, ScriptPurpose(..)
    , Certificate(..)
    , DatumOption(..), KeyValuePair(..), Metadatum(..), NativeScript(..), Redeemer, RedeemerTag(..), Script(..), fromCbor, toCbor
    )

{-| Types and functions related to on-chain transactions.

@docs Transaction

@docs TransactionBody, WitnessSet

@docs Value, MintedValue, PolicyId, adaAssetName

@docs Address, Credential, StakeCredential

@docs Datum, Input, OutputReference, Output

@docs ScriptContext, ScriptPurpose

@docs Certificate

-}

import Bytes exposing (Bytes)
import Cbor.Decode as D
import Cbor.Encode as E
import Debug exposing (todo)
import ElmCardano.Core exposing (Coin, Data, NetworkId(..))
import ElmCardano.Hash exposing (Blake2b_224, Blake2b_256)


{-| A Cardano transaction.
-}
type alias Transaction =
    { body : TransactionBody -- 0
    , witnessSet : WitnessSet -- 1
    , isValid : Bool -- 2
    , auxiliaryData : Maybe AuxiliaryData -- 3
    }


{-| A Cardano transaction body.
-}
type alias TransactionBody =
    { inputs : List Input -- 0
    , outputs : List Output -- 1
    , fee : Int -- 2
    , ttl : Maybe Int -- 3
    , certificates : Maybe (List Certificate) -- 4
    , withdrawals : Maybe (KeyValuePair RewardAccount Coin) -- 5
    , update : Maybe Update -- 6
    , auxiliaryDataHash : Maybe Bytes -- 7
    , validityIntervalStart : Maybe Int -- 8
    , mint : Maybe (Multiasset Coin) -- 9
    , scriptDataHash : Maybe Bytes -- 11
    , collateral : Maybe (List Input) -- 13

    -- TODO: what hash algo for these bytes
    , requiredSigners : Maybe (List Bytes) -- 14
    , networkId : Maybe NetworkId -- 15
    , collateralReturn : Maybe Output -- 16
    , totalCollateral : Maybe Coin -- 17
    , referenceInputs : Maybe (List Input) -- 18
    }


{-| A Cardano transaction witness set.
<https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L763>
-}
type alias WitnessSet =
    { vkeywitness : Maybe (List VKeyWitness) -- 0
    , nativeScripts : Maybe (List NativeScript) -- 1
    , bootstrapWitness : Maybe (List BootstrapWitness) -- 2
    , plutusV1Script : Maybe (List PlutusV1Script) -- 3
    , plutusData : Maybe (List Data) -- 4
    , redeemer : Maybe (List Redeemer) -- 5
    , plutusV2Script : Maybe (List PlutusV2Script) -- 6
    }


type alias AuxiliaryData =
    { metadata : Maybe Metadata -- 0
    , nativeScripts : Maybe (List NativeScript) -- 1
    , plutusScripts : Maybe (List PlutusScript) -- 1
    }


type alias Multiasset a =
    KeyValuePair PolicyId (KeyValuePair AssetName a)


type alias Update =
    { proposedProtocolParameterUpdates : KeyValuePair GenesisHash ProtocolParamUpdate
    , epoch : Epoch
    }


type alias ProtocolParamUpdate =
    { -- #[n(0)]
      minfeeA : Maybe Int
    , -- #[n(1)]
      minfeeB : Maybe Int
    , -- #[n(2)]
      maxBlockBodySize : Maybe Int
    , -- #[n(3)]
      maxTransactionSize : Maybe Int
    , -- #[n(4)]
      maxBlockHeaderSize : Maybe Int
    , -- #[n(5)]
      keyDeposit : Maybe Coin
    , -- #[n(6)]
      poolDeposit : Maybe Coin
    , -- #[n(7)]
      maximumEpoch : Maybe Epoch
    , -- #[n(8)]
      desiredNumberOfStakePools : Maybe Int
    , -- #[n(9)]
      poolPledgeInfluence : Maybe RationalNumber
    , -- #[n(10)]
      expansionRate : Maybe UnitInterval
    , -- #[n(11)]
      treasuryGrowthRate : Maybe UnitInterval
    , -- #[n(14)]
      protocolVersion : Maybe ProtocolVersion
    , -- #[n(16)]
      minPoolCost : Maybe Coin
    , -- #[n(17)]
      adaPerUtxoByte : Maybe Coin
    , -- #[n(18)]
      costModelsForScriptLanguages : Maybe CostModels
    , -- #[n(19)]
      executionCosts : Maybe ExUnitPrices
    , -- #[n(20)]
      maxTxExUnits : Maybe ExUnits
    , -- #[n(21)]
      maxBlockExUnits : Maybe ExUnits
    , -- #[n(22)]
      maxValueSize : Maybe Int
    , -- #[n(23)]
      collateralPercentage : Maybe Int
    , -- #[n(24)]
      maxCollateralInputs : Maybe Int
    }


type alias CostModels =
    { -- #[n(0)]
      plutusV1 : Maybe CostModel
    , -- #[n(1)]
      plutusV2 : Maybe CostModel
    }


type alias CostModel =
    List Int


type alias ExUnitPrices =
    { -- #[n(0)]
      memPrice : PositiveInterval
    , -- #[n(1)]
      stepPrice : PositiveInterval
    }


type alias ProtocolVersion =
    ( Int, Int )


type alias UnitInterval =
    RationalNumber


type alias PositiveInterval =
    RationalNumber



-- https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L379


type alias RationalNumber =
    { numerator : Int
    , denominator : Int
    }


type alias GenesisHash =
    Bytes


type alias Epoch =
    Int


type alias Metadata =
    KeyValuePair MetadatumLabel Metadatum


type alias MetadatumLabel =
    Int


type Metadatum
    = MInt Int
    | MBytes Bytes
    | Text String
    | MList (List Metadatum)
    | Map (KeyValuePair Metadatum Metadatum)


type alias RewardAccount =
    Bytes


type KeyValuePair k v
    = Def (List ( k, v ))
    | Indef (List ( k, v ))


type alias PlutusScript =
    Bytes


type alias PlutusV1Script =
    Bytes


type alias PlutusV2Script =
    Bytes



-- script = [ 0, native_script // 1, plutus_v1_script // 2, plutus_v2_script ]


{-| <https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/babbage/model.rs#L58>
-}
type Script
    = Native NativeScript
    | PlutusV1 PlutusV1Script
    | PlutusV2 PlutusV2Script


type alias VKeyWitness =
    { vkey : Bytes -- 0
    , signature : Bytes
    }


{-| A native script
<https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L772>
-}
type NativeScript
    = ScriptPubkey
        -- TODO: AddrKeyHash
        Bytes
    | ScriptAll (List NativeScript)
    | ScriptAny (List NativeScript)
    | ScriptNofK Int (List NativeScript)
    | InvalidBefore Int
    | InvalidHereafter Int


type alias Redeemer =
    { tag : RedeemerTag -- 0
    , index : Int -- 1
    , data : Data -- 2
    , exUnits : ExUnits -- 3
    }


type RedeemerTag
    = Spend
    | Mint
    | Cert
    | Reward


type alias ExUnits =
    { mem : Int -- 0
    , steps : Int -- 1
    }



-- TODO: what kinds of hashes are these?


type alias BootstrapWitness =
    { publicKey : Bytes -- 0
    , signature : Bytes -- 1
    , chainCode : Bytes -- 2
    , attributes : Bytes -- 3
    }



-- Token Values ################################################################


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

-}
type Value
    = Coin Coin
    | Multiasset Coin (Multiasset Coin)


{-| A multi-asset value that can be found when minting transaction.

Note that because of historical reasons, this is slightly different from Value found in transaction outputs.

-}
type MintedValue
    = MintedValue


{-| The policy id of a Cardano Token. Ada ("") is a special case since it cannot be minted.
-}
type alias PolicyId =
    Blake2b_224


type alias AssetName =
    Bytes


{-| Ada, the native currency, isn’t associated with any AssetName (it’s not possible to mint Ada!).
By convention, it is an empty ByteArray.
-}
adaAssetName : String
adaAssetName =
    ""



-- Credentials #################################################################


{-| A Cardano address typically holding one or two credential references.

Note that legacy bootstrap addresses (a.k.a. "Byron addresses") are completely excluded from Plutus contexts.
Thus, from an on-chain perspective only exists addresses of type 00, 01, …, 07 as detailed in CIP-0019 :: Shelley Addresses.

-}
type alias Address =
    { paymentCredential : Credential
    , stakeCredential : Maybe StakeCredential
    }


{-| A general structure for representing an on-chain credential.

Credentials are always one of two kinds: a direct public/private key pair, or a script (native or Plutus).

-}
type Credential
    = VerificationKeyCredential Blake2b_224
    | ScriptCredential Blake2b_224


{-| A StakeCredential represents the delegation and rewards withdrawal conditions associated with some stake address / account.

A StakeCredential is either provided inline, or, by reference using an on-chain pointer.
Read more about pointers in CIP-0019 :: Pointers.

-}
type StakeCredential
    = InlineCredential Credential
    | PointerCredential { slotNumber : Int, transactionIndex : Int, certificateIndex : Int }



-- Inputs/Outputs ##############################################################


{-| Nickname for data stored in a eUTxO.
-}
type DatumOption
    = DatumHash Blake2b_256
    | Datum Data


{-| An input eUTxO for a transaction.
-}
type alias Input =
    { transactionId : Blake2b_256
    , outputIndex : Int
    }


{-| The reference for a eUTxO.
-}
type alias OutputReference =
    Input


{-| The content of a eUTxO.
-}
type Output
    = Legacy
        { address : Bytes
        , amount : Value
        , datumHash : Maybe Bytes
        }
    | PostAlonzo
        { address : Bytes
        , value : Value
        , datumOption : Maybe DatumOption
        , referenceScript : Maybe Blake2b_224
        }



-- Scripts #####################################################################


{-| A context given to a script by the Cardano ledger when being executed.

The context contains information about the entire transaction that contains the script.
The transaction may also contain other scripts.
To distinguish between multiple scripts, the ScriptContext contains a "purpose" identifying the current resource triggering this execution.

-}
type alias ScriptContext =
    { transaction : Transaction
    , purpose : ScriptPurpose
    }


{-| Characterizes the kind of script being executed and the associated resource.
-}
type ScriptPurpose
    = SPMint PolicyId
    | SPSpend OutputReference
    | SPWithdrawFrom StakeCredential
    | SPPublish Certificate



-- Certificate #################################################################


{-| An on-chain certificate attesting of some operation.
Publishing certificates triggers different kind of rules.
Most of the time, they require signatures from specific keys.
-}
type Certificate
    = CredentialRegistration { delegator : StakeCredential }
    | CredentialDeregistration { delegator : StakeCredential }
    | CredentialDelegation { delegator : StakeCredential, delegatee : Blake2b_224 }
    | PoolRegistration { poolId : Blake2b_224, vrf : Blake2b_224 }
    | PoolDeregistration { poolId : Blake2b_224, epoch : Int }
    | Governance
    | TreasuryMovement



-- https://github.com/input-output-hk/cardano-ledger/blob/a792fbff8156773e712ef875d82c2c6d4358a417/eras/babbage/test-suite/cddl-files/babbage.cddl#L13


toCbor : Transaction -> Bytes
toCbor tx =
    tx |> encodeTransaction |> E.encode


fromCbor : Bytes -> Maybe Transaction
fromCbor bytes =
    D.decode decodeTransaction bytes


encodeTransaction : Transaction -> E.Encoder
encodeTransaction { body, witnessSet, isValid, auxiliaryData } =
    E.sequence
        [ E.beginList
        , encodeTransactionBody body
        , encodeWitnessSet witnessSet
        , E.bool isValid
        , encodeNullable encodeAuxiliaryData auxiliaryData
        , E.break
        ]


encodeTransactionBody : TransactionBody -> E.Encoder
encodeTransactionBody body =
    E.beginDict
        |> encodeField 0 encodeInputs body.inputs
        |> encodeField 1 encodeOutputs body.outputs
        |> encodeField 2 E.int body.fee
        |> encodeFieldMaybe 3 E.int body.ttl
        |> encodeFieldMaybe 4 encodeCertificates body.certificates
        |> encodeFieldMaybe 5 (\_ -> todo "") body.withdrawals
        |> encodeFieldMaybe 6 (\_ -> todo "") body.update
        |> encodeFieldMaybe 7 (\_ -> todo "") body.auxiliaryDataHash
        |> encodeFieldMaybe 8 E.int body.validityIntervalStart
        |> encodeFieldMaybe 9 (\_ -> todo "") body.mint
        |> encodeFieldMaybe 11 E.bytes body.scriptDataHash
        |> encodeFieldMaybe 13 encodeInputs body.collateral
        |> encodeFieldMaybe 14 encodeRequiredSigners body.requiredSigners
        |> encodeFieldMaybe 15 encodeNetworkId body.networkId
        |> encodeFieldMaybe 16 encodeOutput body.collateralReturn
        |> encodeFieldMaybe 17 E.int body.totalCollateral
        |> encodeFieldMaybe 18 encodeInputs body.referenceInputs
        |> (\b -> E.sequence [ b, E.break ])


encodeNetworkId : NetworkId -> E.Encoder
encodeNetworkId networkId =
    E.int <|
        case networkId of
            Testnet ->
                0

            Mainnet ->
                1


encodeWitnessSet : WitnessSet -> E.Encoder
encodeWitnessSet witnessSet =
    E.beginDict
        |> encodeFieldMaybe 0 encodeVKeyWitnesses witnessSet.vkeywitness
        |> encodeFieldMaybe 1 (\scripts -> todo "") witnessSet.nativeScripts
        |> encodeFieldMaybe 2 encodeBootstrapWitnesses witnessSet.bootstrapWitness
        |> encodeFieldMaybe 3 (\scripts -> E.list E.bytes scripts) witnessSet.plutusV1Script
        |> encodeFieldMaybe 4 (\data -> E.list encodeData data) witnessSet.plutusData
        |> encodeFieldMaybe 5 (\redeemers -> E.list encodeRedeemer redeemers) witnessSet.redeemer
        |> encodeFieldMaybe 6 (\scripts -> E.list E.bytes scripts) witnessSet.plutusV2Script
        |> (\b -> E.sequence [ b, E.break ])


encodeVKeyWitnesses : List VKeyWitness -> E.Encoder
encodeVKeyWitnesses v =
    E.list encodeVKeyWitness v


encodeVKeyWitness : VKeyWitness -> E.Encoder
encodeVKeyWitness v =
    E.sequence
        [ E.beginList
        , E.bytes v.vkey
        , E.bytes v.signature
        , E.break
        ]


encodeBootstrapWitnesses : List BootstrapWitness -> E.Encoder
encodeBootstrapWitnesses b =
    E.list encodeBootstrapWitness b


encodeBootstrapWitness : BootstrapWitness -> E.Encoder
encodeBootstrapWitness b =
    E.sequence
        [ E.beginList
        , E.bytes b.publicKey
        , E.bytes b.signature
        , E.break
        ]


encodeAuxiliaryData : AuxiliaryData -> E.Encoder
encodeAuxiliaryData _ =
    todo "encode auxiliary data"


encodeInputs : List Input -> E.Encoder
encodeInputs inputs =
    E.list encodeInput inputs


encodeInput : Input -> E.Encoder
encodeInput { transactionId, outputIndex } =
    E.sequence
        [ E.beginList
        , E.bytes transactionId
        , E.int outputIndex
        , E.break
        ]


encodeOutputs : List Output -> E.Encoder
encodeOutputs outputs =
    E.list encodeOutput outputs


encodeOutput : Output -> E.Encoder
encodeOutput output =
    E.sequence <|
        case output of
            Legacy { address, amount, datumHash } ->
                [ E.beginList
                , E.bytes address
                , encodeValue amount
                , encodeOptional E.bytes datumHash
                , E.break
                ]

            PostAlonzo { address, value, datumOption, referenceScript } ->
                [ E.beginDict
                    |> encodeField 0 E.bytes address
                    |> encodeField 1 encodeValue value
                    |> encodeFieldMaybe 2 encodeDatumOption datumOption
                    |> encodeFieldMaybe 3 (\_ -> todo "") referenceScript
                , E.break
                ]


encodeDatumOption : DatumOption -> E.Encoder
encodeDatumOption datumOption =
    E.sequence <|
        case datumOption of
            DatumHash hash ->
                [ E.beginList
                , E.int 0
                , E.bytes hash
                , E.break
                ]

            Datum datum ->
                [ E.beginList
                , E.int 1
                , encodeData datum
                , E.break
                ]


encodeData : Data -> E.Encoder
encodeData data =
    todo "encode plutus data"


encodeRedeemer : Redeemer -> E.Encoder
encodeRedeemer { tag, index, data, exUnits } =
    E.sequence
        [ E.beginList
        , encodeRedeemerTag tag
        , E.int index
        , encodeData data
        , encodeExUnits exUnits
        , E.break
        ]


encodeRedeemerTag : RedeemerTag -> E.Encoder
encodeRedeemerTag redeemerTag =
    E.int <|
        case redeemerTag of
            Spend ->
                0

            Mint ->
                1

            Cert ->
                2

            Reward ->
                3


encodeExUnits : ExUnits -> E.Encoder
encodeExUnits exUnits =
    E.sequence
        [ E.beginList
        , E.int exUnits.mem
        , E.int exUnits.steps
        , E.break
        ]


encodeCertificates : List Certificate -> E.Encoder
encodeCertificates certificates =
    E.list encodeCertificate certificates


encodeCertificate : Certificate -> E.Encoder
encodeCertificate _ =
    todo "encode certificate"


encodeRequiredSigners : List Bytes -> E.Encoder
encodeRequiredSigners requiredSigners =
    E.list E.bytes requiredSigners


encodeValue : Value -> E.Encoder
encodeValue value =
    case value of
        Coin amount ->
            E.int amount

        Multiasset amount multiasset ->
            E.sequence
                [ E.beginList
                , E.int amount
                , todo "encode multiasset"
                , E.break
                ]


{-| Encode things shown in the cddl as `x // null`.
-}
encodeNullable : (a -> E.Encoder) -> Maybe a -> E.Encoder
encodeNullable apply value =
    encodeMaybe apply E.null value


encodeOptional : (a -> E.Encoder) -> Maybe a -> E.Encoder
encodeOptional apply value =
    encodeMaybe apply (E.sequence []) value


encodeField : Int -> (a -> E.Encoder) -> a -> E.Encoder -> E.Encoder
encodeField ix encode a e =
    E.sequence [ e, E.pair E.int encode ( ix, a ) ]


encodeFieldMaybe : Int -> (a -> E.Encoder) -> Maybe a -> E.Encoder -> E.Encoder
encodeFieldMaybe ix encode maybe =
    case maybe of
        Nothing ->
            identity

        Just a ->
            encodeField ix encode a


encodeMaybe : (a -> E.Encoder) -> E.Encoder -> Maybe a -> E.Encoder
encodeMaybe apply default value =
    value
        |> Maybe.map apply
        |> Maybe.withDefault default


decodeTransaction : D.Decoder Transaction
decodeTransaction =
    todo "decode tx"
