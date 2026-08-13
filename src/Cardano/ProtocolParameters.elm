module Cardano.ProtocolParameters exposing
    ( ProtocolParameters, ValidationError(..)
    , defaultProtocolParameters, validate
    )

{-| Protocol parameters used when building and validating transactions.


## Parameters

@docs ProtocolParameters, ValidationError


## Defaults and validation

@docs defaultProtocolParameters, validate

-}

import Cardano.Gov as Gov exposing (CostModels, DrepVotingThresholds, PoolVotingThresholds)
import Cardano.Redeemer exposing (ExUnitPrices, ExUnits)
import Cardano.Transaction as Transaction
import Cardano.Uplc as Uplc
import Cardano.Utils exposing (RationalNumber, UnitInterval)
import Natural exposing (Natural)


{-| Protocol parameters used by transaction building and validation.

Monetary quantities use unbounded natural numbers. A few ledger unsigned integer
types use `Int` through existing public types and are checked by [validate].

-}
type alias ProtocolParameters =
    { minFeeA : Natural
    , minFeeB : Natural
    , maxBlockBodySize : Int
    , maxTransactionSize : Int
    , maxBlockHeaderSize : Int
    , keyDeposit : Natural
    , poolDeposit : Natural
    , maximumEpoch : Natural
    , desiredNumberOfStakePools : Int
    , poolPledgeInfluence : RationalNumber
    , expansionRate : UnitInterval
    , treasuryGrowthRate : UnitInterval
    , minPoolCost : Natural
    , adaPerUtxoByte : Natural
    , costModels : CostModels
    , executionCosts : ExUnitPrices
    , maxTxExUnits : ExUnits
    , maxBlockExUnits : ExUnits
    , maxValueSize : Int
    , collateralPercentage : Int
    , maxCollateralInputs : Int
    , poolVotingThresholds : PoolVotingThresholds
    , drepVotingThresholds : DrepVotingThresholds
    , minCommitteeSize : Int
    , committeeTermLimit : Natural
    , governanceActionValidityPeriod : Natural
    , governanceActionDeposit : Natural
    , drepDeposit : Natural
    , drepInactivityPeriod : Natural
    , minFeeRefScriptCostPerByte : RationalNumber
    }


{-| Why a protocol parameter set failed validation.

The `parameter` field identifies the invalid record field, including nested
threshold and execution-price fields.

-}
type ValidationError
    = InvalidIntegerParameter { parameter : String, value : Int }
    | InvalidExecutionUnitsParameter { parameter : String, value : ExUnits }
    | InvalidRationalParameter { parameter : String, value : RationalNumber }
    | InvalidUnitIntervalParameter { parameter : String, value : UnitInterval }


{-| A static protocol-parameter set used by the library's default
transaction finalizers.

These values are compiled into the library. They are not fetched from a network,
so callers targeting another network or protocol-parameter epoch should provide
and validate their own set.

-}
defaultProtocolParameters : ProtocolParameters
defaultProtocolParameters =
    { minFeeA = Transaction.defaultTxFeeParams.feePerByte
    , minFeeB = Transaction.defaultTxFeeParams.baseFee
    , maxBlockBodySize = 90112
    , maxTransactionSize = Gov.defaultMaxTxSize
    , maxBlockHeaderSize = 1100
    , keyDeposit = Natural.fromSafeInt 2000000
    , poolDeposit = Natural.fromSafeInt 500000000
    , maximumEpoch = Natural.fromSafeInt 18
    , desiredNumberOfStakePools = 500
    , poolPledgeInfluence = { numerator = 3, denominator = 10 }
    , expansionRate = { numerator = 3, denominator = 1000 }
    , treasuryGrowthRate = { numerator = 1, denominator = 5 }
    , minPoolCost = Natural.fromSafeInt 170000000
    , adaPerUtxoByte = Natural.fromSafeInt 4310
    , costModels = Uplc.conwayDefaultCostModels
    , executionCosts = Transaction.defaultTxFeeParams.scriptExUnitPrice
    , maxTxExUnits = Uplc.conwayDefaultBudget
    , maxBlockExUnits = { mem = 62000000, steps = 40000000000 }
    , maxValueSize = 5000
    , collateralPercentage = 150
    , maxCollateralInputs = 3
    , poolVotingThresholds =
        { motionNoConfidence = { numerator = 51, denominator = 100 }
        , committeeNormal = { numerator = 51, denominator = 100 }
        , committeeNoConfidence = { numerator = 51, denominator = 100 }
        , hardforkInitiation = { numerator = 51, denominator = 100 }
        , securityRelevantParameter = { numerator = 51, denominator = 100 }
        }
    , drepVotingThresholds =
        { motionNoConfidence = { numerator = 67, denominator = 100 }
        , committeeNormal = { numerator = 67, denominator = 100 }
        , committeeNoConfidence = { numerator = 6, denominator = 10 }
        , updateConstitution = { numerator = 75, denominator = 100 }
        , hardforkInitiation = { numerator = 6, denominator = 10 }
        , ppNetworkGroup = { numerator = 67, denominator = 100 }
        , ppEconomicGroup = { numerator = 67, denominator = 100 }
        , ppTechnicalGroup = { numerator = 67, denominator = 100 }
        , ppGovernanceGroup = { numerator = 75, denominator = 100 }
        , treasuryWithdrawal = { numerator = 67, denominator = 100 }
        }
    , minCommitteeSize = 0
    , committeeTermLimit = Natural.fromSafeInt 146
    , governanceActionValidityPeriod = Natural.fromSafeInt 6
    , governanceActionDeposit = Natural.fromSafeInt 100000000000
    , drepDeposit = Natural.fromSafeInt 500000000
    , drepInactivityPeriod = Natural.fromSafeInt 20
    , minFeeRefScriptCostPerByte = Transaction.defaultTxFeeParams.refScriptFeeParams.minFeeRefScriptCostPerByte
    }


{-| Validate the fields that are represented by signed integers or unrestricted
rational numbers in Elm but are unsigned or constrained in the ledger.

Validation returns the original parameter set unchanged on success.

-}
validate : ProtocolParameters -> Result ValidationError ProtocolParameters
validate parameters =
    validationErrors parameters
        |> List.head
        |> Maybe.map Err
        |> Maybe.withDefault (Ok parameters)


validationErrors : ProtocolParameters -> List ValidationError
validationErrors parameters =
    List.filterMap identity
        [ nonNegativeInt "maxBlockBodySize" parameters.maxBlockBodySize
        , nonNegativeInt "maxTransactionSize" parameters.maxTransactionSize
        , nonNegativeInt "maxBlockHeaderSize" parameters.maxBlockHeaderSize
        , nonNegativeInt "desiredNumberOfStakePools" parameters.desiredNumberOfStakePools
        , nonNegativeInt "maxValueSize" parameters.maxValueSize
        , nonNegativeInt "collateralPercentage" parameters.collateralPercentage
        , nonNegativeInt "maxCollateralInputs" parameters.maxCollateralInputs
        , nonNegativeInt "minCommitteeSize" parameters.minCommitteeSize
        , nonNegativeExUnits "maxTxExUnits" parameters.maxTxExUnits
        , nonNegativeExUnits "maxBlockExUnits" parameters.maxBlockExUnits
        , nonNegativeRational "poolPledgeInfluence" parameters.poolPledgeInfluence
        , nonNegativeRational "executionCosts.memPrice" parameters.executionCosts.memPrice
        , nonNegativeRational "executionCosts.stepPrice" parameters.executionCosts.stepPrice
        , nonNegativeRational "minFeeRefScriptCostPerByte" parameters.minFeeRefScriptCostPerByte
        , unitInterval "expansionRate" parameters.expansionRate
        , unitInterval "treasuryGrowthRate" parameters.treasuryGrowthRate
        , unitInterval "poolVotingThresholds.motionNoConfidence" parameters.poolVotingThresholds.motionNoConfidence
        , unitInterval "poolVotingThresholds.committeeNormal" parameters.poolVotingThresholds.committeeNormal
        , unitInterval "poolVotingThresholds.committeeNoConfidence" parameters.poolVotingThresholds.committeeNoConfidence
        , unitInterval "poolVotingThresholds.hardforkInitiation" parameters.poolVotingThresholds.hardforkInitiation
        , unitInterval "poolVotingThresholds.securityRelevantParameter" parameters.poolVotingThresholds.securityRelevantParameter
        , unitInterval "drepVotingThresholds.motionNoConfidence" parameters.drepVotingThresholds.motionNoConfidence
        , unitInterval "drepVotingThresholds.committeeNormal" parameters.drepVotingThresholds.committeeNormal
        , unitInterval "drepVotingThresholds.committeeNoConfidence" parameters.drepVotingThresholds.committeeNoConfidence
        , unitInterval "drepVotingThresholds.updateConstitution" parameters.drepVotingThresholds.updateConstitution
        , unitInterval "drepVotingThresholds.hardforkInitiation" parameters.drepVotingThresholds.hardforkInitiation
        , unitInterval "drepVotingThresholds.ppNetworkGroup" parameters.drepVotingThresholds.ppNetworkGroup
        , unitInterval "drepVotingThresholds.ppEconomicGroup" parameters.drepVotingThresholds.ppEconomicGroup
        , unitInterval "drepVotingThresholds.ppTechnicalGroup" parameters.drepVotingThresholds.ppTechnicalGroup
        , unitInterval "drepVotingThresholds.ppGovernanceGroup" parameters.drepVotingThresholds.ppGovernanceGroup
        , unitInterval "drepVotingThresholds.treasuryWithdrawal" parameters.drepVotingThresholds.treasuryWithdrawal
        ]


nonNegativeInt : String -> Int -> Maybe ValidationError
nonNegativeInt parameter value =
    if value < 0 then
        Just (InvalidIntegerParameter { parameter = parameter, value = value })

    else
        Nothing


nonNegativeExUnits : String -> ExUnits -> Maybe ValidationError
nonNegativeExUnits parameter value =
    if value.mem < 0 || value.steps < 0 then
        Just (InvalidExecutionUnitsParameter { parameter = parameter, value = value })

    else
        Nothing


nonNegativeRational : String -> RationalNumber -> Maybe ValidationError
nonNegativeRational parameter value =
    if value.numerator < 0 || value.denominator <= 0 then
        Just (InvalidRationalParameter { parameter = parameter, value = value })

    else
        Nothing


unitInterval : String -> UnitInterval -> Maybe ValidationError
unitInterval parameter value =
    if value.denominator <= 0 || value.numerator < 0 || value.numerator > value.denominator then
        Just (InvalidUnitIntervalParameter { parameter = parameter, value = value })

    else
        Nothing
