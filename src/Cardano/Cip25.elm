module Cardano.Cip25 exposing
    ( Cip25, PolicyMetadata, AssetMetadata
    , File, ImageMime, MimeType, Uri
    , singleton, insertAssetMetadata, getAllMetadata, getAssetMetadata, assetMetadata, withFile, file, label
    , fromCbor, toCbor, fromMetadatum, toMetadatum
    , imageMimeFromString, imageMimeToMimeType, imageMimeToString
    , mimeTypeFromString, mimeTypeToString
    )

{-| CIP-0025 support.

CIP-0025 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025)
a standard format for metadata of minting transactions so that off-chain tools
can associate a set of information with the tokens originating (or updating) in
that transaction.

@docs Cip25, PolicyMetadata, AssetMetadata

@docs File, ImageMime, MimeType, Uri

@docs singleton, insertAssetMetadata, getAllMetadata, getAssetMetadata, assetMetadata, withFile, file, label

@docs fromCbor, toCbor, fromMetadatum, toMetadatum

@docs imageMimeFromString, imageMimeToMimeType, imageMimeToString
@docs mimeTypeFromString, mimeTypeToString

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as BytesMap exposing (BytesMap)
import Cardano.Metadatum as Metadatum exposing (Metadatum)
import Cardano.MultiAsset as MultiAsset exposing (AssetName, PolicyId)
import Cbor.Decode as D
import Cbor.Encode as E
import Dict exposing (Dict)
import Integer
import Maybe.Extra
import Natural exposing (Natural)


{-| Datatype for modeling CIP-0025.

CIP-0025 metadata is grouped by policy ID, then by asset name. The `version`
field applies to the whole label-721 payload, not to each asset.

-}
type Cip25
    = Cip25
        { version : Version
        , policies : BytesMap PolicyId PolicyMetadata
        }


{-| Metadata for all assets under one policy ID.
-}
type alias PolicyMetadata =
    BytesMap AssetName AssetMetadata


{-| Metadata for a single asset.

The standard simply lays out a set of fields, some of which are optional.
The `image` field is a URI that points to a resource with MIME type `image/*`.
Inline images are represented as `data:` URIs.

TODO: A structured URI model is probably a better design here, so callers can
inspect URI parts before deciding whether to fetch or render an image.

-}
type alias AssetMetadata =
    { name : String
    , image : Uri
    , mediaType : Maybe ImageMime
    , description : Maybe String
    , files : List File
    , otherProps : Dict String Metadatum
    }


{-| Raw URI string.
-}
type alias Uri =
    String


{-| A MIME media type, such as `image/png` or `application/json`.

The constructor is intentionally opaque so this type can represent future IANA
registrations without hard-coding today's registry into the package.

-}
type MimeType
    = MimeType String


{-| A MIME media type constrained to the `image/*` top-level type.

The constructor is intentionally opaque for the same reason as [MimeType]:
image subtypes can be added to the IANA registry over time.

-}
type ImageMime
    = ImageMime String


{-| Opaque datatype to flag CIP-25 version.

V1 metadata can appear on-chain with no version field, with an integer
version, or with string versions such as "1" and "1.0". The string variants are
not CIP-25 canonical, but they are slightly dominant on mainnet, so we accept
them for compatibility. Since V1 values are only created by decoding, the
internal version keeps the decoded wire shape so `fromCbor >> toCbor` can
preserve existing payloads.

-}
type Version
    = V1Implicit
    | V1String String
    | V1Int
    | V2


{-| Datatype to represent optional files specified for an asset.
-}
type alias File =
    { name : String
    , mediaType : MimeType
    , src : Uri
    , otherProps : Dict String Metadatum
    }


{-| CIP-0025 metadata label.
-}
label : Natural
label =
    Natural.fromSafeInt 721


{-| Create CIP-0025 metadata for one asset.

Smart constructors create version 2 metadata. Returns `Nothing` unless the
policy ID is exactly 28 bytes and the asset name is at most 32 bytes.

-}
singleton : Bytes PolicyId -> Bytes AssetName -> AssetMetadata -> Maybe Cip25
singleton policyId assetName metadata =
    insertAssetMetadata policyId assetName metadata <|
        Cip25
            { version = V2
            , policies = BytesMap.empty
            }


{-| Insert or replace one asset's metadata.

Returns `Nothing` for invalid policy IDs, invalid asset names, or non-UTF-8
asset names in version 1 metadata.

-}
insertAssetMetadata : Bytes PolicyId -> Bytes AssetName -> AssetMetadata -> Cip25 -> Maybe Cip25
insertAssetMetadata policyId assetName metadata (Cip25 cip25) =
    if MultiAsset.isValidPolicyId policyId && MultiAsset.isValidAssetName assetName then
        case cip25.version of
            V1Implicit ->
                if Bytes.toText assetName /= Nothing then
                    Just <|
                        Cip25
                            { version = V1Implicit
                            , policies = insertAssetMetadataInPolicyMap policyId assetName metadata cip25.policies
                            }

                else
                    Nothing

            V1String version ->
                if Bytes.toText assetName /= Nothing then
                    Just <|
                        Cip25
                            { version = V1String version
                            , policies = insertAssetMetadataInPolicyMap policyId assetName metadata cip25.policies
                            }

                else
                    Nothing

            V1Int ->
                if Bytes.toText assetName /= Nothing then
                    Just <|
                        Cip25
                            { version = V1Int
                            , policies = insertAssetMetadataInPolicyMap policyId assetName metadata cip25.policies
                            }

                else
                    Nothing

            V2 ->
                Just <|
                    Cip25
                        { version = V2
                        , policies = insertAssetMetadataInPolicyMap policyId assetName metadata cip25.policies
                        }

    else
        Nothing


insertAssetMetadataInPolicyMap :
    Bytes PolicyId
    -> Bytes AssetName
    -> AssetMetadata
    -> BytesMap PolicyId PolicyMetadata
    -> BytesMap PolicyId PolicyMetadata
insertAssetMetadataInPolicyMap policyId assetName metadata policies =
    BytesMap.update policyId
        (\maybePolicyMetadata ->
            maybePolicyMetadata
                |> Maybe.withDefault BytesMap.empty
                |> BytesMap.insert assetName metadata
                |> Just
        )
        policies


{-| Get all asset metadata grouped by policy ID.
-}
getAllMetadata : Cip25 -> BytesMap PolicyId PolicyMetadata
getAllMetadata (Cip25 cip25) =
    cip25.policies


{-| Get metadata for one asset.
-}
getAssetMetadata : Bytes PolicyId -> Bytes AssetName -> Cip25 -> Maybe AssetMetadata
getAssetMetadata policyId assetName (Cip25 cip25) =
    cip25.policies
        |> BytesMap.get policyId
        |> Maybe.andThen (BytesMap.get assetName)


{-| Create asset metadata with optional fields empty.
-}
assetMetadata : String -> Uri -> AssetMetadata
assetMetadata name image =
    { name = name
    , image = image
    , mediaType = Nothing
    , description = Nothing
    , files = []
    , otherProps = Dict.empty
    }


{-| Convert CIP-0025 to the transaction metadatum that belongs under label 721.
-}
toMetadatum : Cip25 -> Metadatum
toMetadatum (Cip25 cip25) =
    let
        v1PolicyEntries =
            cip25.policies
                |> BytesMap.toList
                |> List.map
                    (\( policyId, policyMetadata ) ->
                        ( Metadatum.String (Bytes.toHex policyId)
                        , policyMetadata
                            |> BytesMap.toList
                            |> List.map v1AssetMetadataToMetadatum
                            |> Metadatum.Map
                        )
                    )

        v1ToMetadatum versionEntries =
            Metadatum.Map <|
                v1PolicyEntries
                    ++ versionEntries
    in
    case cip25.version of
        V1Implicit ->
            v1ToMetadatum []

        V1String version ->
            v1ToMetadatum [ ( Metadatum.String "version", Metadatum.String version ) ]

        V1Int ->
            v1ToMetadatum [ ( Metadatum.String "version", Metadatum.Int (Integer.fromSafeInt 1) ) ]

        V2 ->
            Metadatum.Map <|
                (cip25.policies
                    |> BytesMap.toList
                    |> List.map
                        (\( policyId, policyMetadata ) ->
                            ( Metadatum.Bytes (Bytes.toAny policyId)
                            , policyMetadata
                                |> BytesMap.toList
                                |> List.map v2AssetMetadataToMetadatum
                                |> Metadatum.Map
                            )
                        )
                )
                    ++ [ ( Metadatum.String "version", Metadatum.Int (Integer.fromSafeInt 2) ) ]


v1AssetMetadataToMetadatum : ( Bytes AssetName, AssetMetadata ) -> ( Metadatum, Metadatum )
v1AssetMetadataToMetadatum ( assetName, metadata ) =
    ( assetName
        |> Bytes.toText
        |> Maybe.withDefault ""
        |> Metadatum.String
    , assetMetadataToMetadatum metadata
    )


v2AssetMetadataToMetadatum : ( Bytes AssetName, AssetMetadata ) -> ( Metadatum, Metadatum )
v2AssetMetadataToMetadatum ( assetName, metadata ) =
    ( Metadatum.Bytes (Bytes.toAny assetName)
    , assetMetadataToMetadatum metadata
    )


{-| Encode the CIP-0025 payload that belongs under metadata label 721.
-}
toCbor : Cip25 -> E.Encoder
toCbor =
    toMetadatum >> Metadatum.toCbor


{-| Decode the CIP-0025 payload from metadata label 721.
-}
fromCbor : D.Decoder Cip25
fromCbor =
    Metadatum.fromCbor
        |> D.andThen
            (fromMetadatum
                >> Maybe.map D.succeed
                >> Maybe.withDefault D.fail
            )


{-| Convert the transaction metadatum under label 721 to CIP-0025.
-}
fromMetadatum : Metadatum -> Maybe Cip25
fromMetadatum metadatum =
    case metadatum of
        Metadatum.Map pairs ->
            case versionFromMetadatumPairs pairs of
                Just V1Implicit ->
                    decodeV1Policies (withoutVersion pairs)
                        |> Maybe.map (\policies -> Cip25 { version = V1Implicit, policies = policies })

                Just (V1String version) ->
                    decodeV1Policies (withoutVersion pairs)
                        |> Maybe.map (\policies -> Cip25 { version = V1String version, policies = policies })

                Just V1Int ->
                    decodeV1Policies (withoutVersion pairs)
                        |> Maybe.map (\policies -> Cip25 { version = V1Int, policies = policies })

                Just V2 ->
                    decodeV2Policies (withoutVersion pairs)
                        |> Maybe.map (\policies -> Cip25 { version = V2, policies = policies })

                Nothing ->
                    Nothing

        _ ->
            Nothing


versionFromMetadatumPairs : List ( Metadatum, Metadatum ) -> Maybe Version
versionFromMetadatumPairs pairs =
    case List.filter (Tuple.first >> isVersionKey) pairs of
        [] ->
            Just V1Implicit

        [ ( _, version ) ] ->
            case version of
                Metadatum.Int value ->
                    if value == Integer.one then
                        Just V1Int

                    else if value == Integer.two then
                        Just V2

                    else
                        Nothing

                Metadatum.String "1" ->
                    Just (V1String "1")

                Metadatum.String "1.0" ->
                    Just (V1String "1.0")

                _ ->
                    Nothing

        _ ->
            Nothing


withoutVersion : List ( Metadatum, Metadatum ) -> List ( Metadatum, Metadatum )
withoutVersion =
    List.filter (Tuple.first >> isVersionKey >> not)


isVersionKey : Metadatum -> Bool
isVersionKey key =
    key == Metadatum.String "version"


decodeV1Policies : List ( Metadatum, Metadatum ) -> Maybe (BytesMap PolicyId PolicyMetadata)
decodeV1Policies pairs =
    Maybe.Extra.combineMap decodeV1Policy pairs
        |> Maybe.map BytesMap.fromList


decodeV1Policy : ( Metadatum, Metadatum ) -> Maybe ( Bytes PolicyId, PolicyMetadata )
decodeV1Policy pair =
    case pair of
        ( Metadatum.String policyIdHex, Metadatum.Map policyMetadata ) ->
            case Bytes.fromHex policyIdHex of
                Just policyId ->
                    if MultiAsset.isValidPolicyId policyId then
                        Maybe.Extra.combineMap decodeV1AssetMetadata policyMetadata
                            |> Maybe.map (\assets -> ( policyId, BytesMap.fromList assets ))

                    else
                        Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing


decodeV1AssetMetadata : ( Metadatum, Metadatum ) -> Maybe ( Bytes AssetName, AssetMetadata )
decodeV1AssetMetadata pair =
    case pair of
        ( Metadatum.String assetNameText, metadatum ) ->
            let
                assetName =
                    Bytes.fromText assetNameText
            in
            if MultiAsset.isValidAssetName assetName then
                assetMetadataFromMetadatum metadatum
                    |> Maybe.map (\metadata -> ( assetName, metadata ))

            else
                Nothing

        _ ->
            Nothing


decodeV2Policies : List ( Metadatum, Metadatum ) -> Maybe (BytesMap PolicyId PolicyMetadata)
decodeV2Policies pairs =
    Maybe.Extra.combineMap decodeV2Policy pairs
        |> Maybe.map BytesMap.fromList


decodeV2Policy : ( Metadatum, Metadatum ) -> Maybe ( Bytes PolicyId, PolicyMetadata )
decodeV2Policy pair =
    case pair of
        ( Metadatum.Bytes bytes, Metadatum.Map policyMetadata ) ->
            let
                policyId =
                    changeBytesType bytes
            in
            if MultiAsset.isValidPolicyId policyId then
                Maybe.Extra.combineMap decodeV2AssetMetadata policyMetadata
                    |> Maybe.map (\assets -> ( policyId, BytesMap.fromList assets ))

            else
                Nothing

        _ ->
            Nothing


decodeV2AssetMetadata : ( Metadatum, Metadatum ) -> Maybe ( Bytes AssetName, AssetMetadata )
decodeV2AssetMetadata pair =
    case pair of
        ( Metadatum.Bytes bytes, metadatum ) ->
            let
                assetName =
                    changeBytesType bytes
            in
            if MultiAsset.isValidAssetName assetName then
                assetMetadataFromMetadatum metadatum
                    |> Maybe.map (\metadata -> ( assetName, metadata ))

            else
                Nothing

        _ ->
            Nothing


changeBytesType : Bytes a -> Bytes b
changeBytesType =
    Bytes.toBytes >> Bytes.fromBytes


{-| Convert CIP-0025 asset metadata to transaction metadatum.
-}
assetMetadataToMetadatum : AssetMetadata -> Metadatum
assetMetadataToMetadatum metadata =
    let
        otherProps =
            metadata.otherProps
                |> Dict.filter
                    (\key _ ->
                        not (List.member key [ "name", "image", "mediaType", "description", "files" ])
                    )
                |> Dict.toList
                |> List.map (Tuple.mapFirst Metadatum.String)

        optionalMediaType =
            metadata.mediaType
                |> Maybe.map
                    (\mediaType ->
                        ( Metadatum.String "mediaType"
                        , mediaType |> imageMimeToString |> stringToMetadatum
                        )
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        optionalDescription =
            metadata.description
                |> Maybe.map
                    (\description ->
                        ( Metadatum.String "description"
                        , stringToMetadatum description
                        )
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        optionalFiles =
            if List.isEmpty metadata.files then
                []

            else
                [ ( Metadatum.String "files"
                  , Metadatum.List (List.map fileToMetadatum metadata.files)
                  )
                ]
    in
    Metadatum.Map <|
        [ ( Metadatum.String "name", stringToMetadatum metadata.name )
        , ( Metadatum.String "image", stringToMetadatum metadata.image )
        ]
            ++ optionalMediaType
            ++ optionalDescription
            ++ optionalFiles
            ++ otherProps


assetMetadataFromMetadatum : Metadatum -> Maybe AssetMetadata
assetMetadataFromMetadatum metadatum =
    let
        step ( keyMetadatum, value ) fields =
            case keyMetadatum of
                Metadatum.String key ->
                    case key of
                        "name" ->
                            case stringFromMetadatum value of
                                Just name ->
                                    { fields | name = Just name }

                                Nothing ->
                                    { fields | invalid = True }

                        "image" ->
                            case stringFromMetadatum value of
                                Just image ->
                                    { fields | image = Just image }

                                Nothing ->
                                    { fields | invalid = True }

                        "mediaType" ->
                            case stringFromMetadatum value |> Maybe.andThen imageMimeFromString of
                                Just mediaType ->
                                    { fields | mediaType = Just mediaType }

                                Nothing ->
                                    { fields | invalid = True }

                        "description" ->
                            case stringFromMetadatum value of
                                Just description ->
                                    { fields | description = Just description }

                                Nothing ->
                                    { fields | invalid = True }

                        "files" ->
                            case value of
                                Metadatum.List files ->
                                    case Maybe.Extra.combineMap fileFromMetadatum files of
                                        Just validFiles ->
                                            { fields | files = validFiles }

                                        Nothing ->
                                            { fields | invalid = True }

                                _ ->
                                    { fields | invalid = True }

                        _ ->
                            { fields | otherProps = Dict.insert key value fields.otherProps }

                _ ->
                    { fields | invalid = True }
    in
    case metadatum of
        Metadatum.Map pairs ->
            let
                fields =
                    List.foldl step
                        { name = Nothing
                        , image = Nothing
                        , mediaType = Nothing
                        , description = Nothing
                        , files = []
                        , otherProps = Dict.empty
                        , invalid = False
                        }
                        pairs
            in
            case ( fields.invalid, fields.name, fields.image ) of
                ( False, Just name, Just image ) ->
                    Just
                        { name = name
                        , image = image
                        , mediaType = fields.mediaType
                        , description = fields.description
                        , files = fields.files
                        , otherProps = fields.otherProps
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Add a file to asset metadata.
-}
withFile : File -> AssetMetadata -> AssetMetadata
withFile file_ metadata =
    { metadata | files = metadata.files ++ [ file_ ] }


{-| Create file metadata with optional fields empty.

Returns `Nothing` if the MIME type string is invalid.

When encoded to CBOR, entries in `otherProps` with keys `"name"`,
`"mediaType"`, or `"src"` are ignored. Those fields are always encoded from the
dedicated record fields.

-}
file : String -> String -> Uri -> Maybe File
file name mediaType src =
    mimeTypeFromString mediaType
        |> Maybe.map
            (\validMediaType ->
                { name = name
                , mediaType = validMediaType
                , src = src
                , otherProps = Dict.empty
                }
            )


{-| Convert CIP-0025 file metadata to transaction metadatum.
-}
fileToMetadatum : File -> Metadatum
fileToMetadatum file_ =
    let
        otherProps =
            file_.otherProps
                |> Dict.filter (\key _ -> key /= "name" && key /= "mediaType" && key /= "src")
                |> Dict.toList
                |> List.map (Tuple.mapFirst Metadatum.String)
    in
    Metadatum.Map <|
        [ ( Metadatum.String "name", stringToMetadatum file_.name )
        , ( Metadatum.String "mediaType", file_.mediaType |> mimeTypeToString |> stringToMetadatum )
        , ( Metadatum.String "src", stringToMetadatum file_.src )
        ]
            ++ otherProps


fileFromMetadatum : Metadatum -> Maybe File
fileFromMetadatum metadatum =
    let
        step ( keyMetadatum, value ) fields =
            case keyMetadatum of
                Metadatum.String key ->
                    case key of
                        "name" ->
                            { fields | name = stringFromMetadatum value }

                        "mediaType" ->
                            { fields | mediaType = stringFromMetadatum value |> Maybe.andThen mimeTypeFromString }

                        "src" ->
                            { fields | src = stringFromMetadatum value }

                        _ ->
                            { fields | otherProps = Dict.insert key value fields.otherProps }

                _ ->
                    { fields | invalid = True }
    in
    case metadatum of
        Metadatum.Map pairs ->
            let
                fields =
                    List.foldl step
                        { name = Nothing
                        , mediaType = Nothing
                        , src = Nothing
                        , otherProps = Dict.empty
                        , invalid = False
                        }
                        pairs
            in
            if fields.invalid then
                Nothing

            else
                case ( fields.name, fields.mediaType, fields.src ) of
                    ( Just name, Just mediaType, Just src ) ->
                        Just
                            { name = name
                            , mediaType = mediaType
                            , src = src
                            , otherProps = fields.otherProps
                            }

                    _ ->
                        Nothing

        _ ->
            Nothing


{-| Build a MIME media type from a full `type/subtype` string.

This validates the two name components and stores the media type in lowercase.

-}
mimeTypeFromString : String -> Maybe MimeType
mimeTypeFromString value =
    case String.split "/" value of
        [ type_, subtype ] ->
            if isValidMimeTypeName type_ && isValidMimeTypeName subtype then
                Just (MimeType (String.toLower value))

            else
                Nothing

        _ ->
            Nothing


{-| Convert a MIME media type back to its original string representation.
-}
mimeTypeToString : MimeType -> String
mimeTypeToString (MimeType value) =
    value


{-| Check whether a string is a valid registered MIME type name component.

This validates one side of `type/subtype`, such as `image` or `svg+xml`,
using the [RFC 6838](https://www.iana.org/go/rfc6838) `restricted-name`
grammar.

-}
isValidMimeTypeName : String -> Bool
isValidMimeTypeName name =
    if String.isEmpty name || String.length name > 127 then
        False

    else
        case String.uncons name of
            Nothing ->
                False

            Just ( first, rest ) ->
                (((first >= 'A') && (first <= 'Z'))
                    || ((first >= 'a') && (first <= 'z'))
                    || ((first >= '0') && (first <= '9'))
                )
                    && String.all
                        (\char ->
                            ((char >= 'A') && (char <= 'Z'))
                                || ((char >= 'a') && (char <= 'z'))
                                || ((char >= '0') && (char <= '9'))
                                || (char == '!')
                                || (char == '#')
                                || (char == '$')
                                || (char == '&')
                                || (char == '-')
                                || (char == '^')
                                || (char == '_')
                                || (char == '.')
                                || (char == '+')
                        )
                        rest


{-| Build an image MIME media type from a full `image/subtype` string.

This validates the two name components and stores the subtype in lowercase.

-}
imageMimeFromString : String -> Maybe ImageMime
imageMimeFromString value =
    case String.split "/" value of
        [ type_, subtype ] ->
            if
                (String.toLower type_ == "image")
                    && isValidMimeTypeName type_
                    && isValidMimeTypeName subtype
            then
                Just (ImageMime (String.toLower subtype))

            else
                Nothing

        _ ->
            Nothing


{-| Convert an image MIME media type into the more general MIME type.
-}
imageMimeToMimeType : ImageMime -> MimeType
imageMimeToMimeType =
    imageMimeToString >> MimeType


{-| Convert an image MIME media type back to its original string representation.
-}
imageMimeToString : ImageMime -> String
imageMimeToString (ImageMime subtype) =
    "image/" ++ subtype


stringFromMetadatum : Metadatum -> Maybe String
stringFromMetadatum metadatum =
    let
        boundedString string =
            if utf8Width string <= 64 then
                Just string

            else
                Nothing
    in
    case metadatum of
        Metadatum.String string ->
            boundedString string

        Metadatum.List chunks ->
            chunks
                |> Maybe.Extra.combineMap
                    (\chunk ->
                        case chunk of
                            Metadatum.String string ->
                                boundedString string

                            _ ->
                                Nothing
                    )
                |> Maybe.map String.concat

        _ ->
            Nothing


{-| Convert a CIP-0025 string to transaction metadatum.

Strings longer than 64 bytes are encoded as a list of 64-byte string chunks.

-}
stringToMetadatum : String -> Metadatum
stringToMetadatum string =
    case chunksOfBytes 64 string of
        [] ->
            Metadatum.String ""

        [ chunk ] ->
            Metadatum.String chunk

        chunks ->
            Metadatum.List (List.map Metadatum.String chunks)


utf8Width : String -> Int
utf8Width =
    String.foldl (\char width -> width + utf8CharWidth char) 0


utf8CharWidth : Char -> Int
utf8CharWidth char =
    let
        code =
            Char.toCode char
    in
    if code <= 127 then
        1

    else if code <= 2047 then
        2

    else if code <= 65535 then
        3

    else
        4


chunksOfBytes : Int -> String -> List String
chunksOfBytes maxBytes string =
    let
        step char ( current, currentWidth, chunks ) =
            let
                charString =
                    String.fromChar char

                charWidth =
                    utf8CharWidth char
            in
            if current == "" then
                ( charString, charWidth, chunks )

            else if currentWidth + charWidth > maxBytes then
                ( charString, charWidth, current :: chunks )

            else
                ( current ++ charString, currentWidth + charWidth, chunks )
    in
    if maxBytes <= 0 then
        []

    else
        case String.foldl step ( "", 0, [] ) string of
            ( "", _, chunks ) ->
                List.reverse chunks

            ( current, _, chunks ) ->
                List.reverse (current :: chunks)
