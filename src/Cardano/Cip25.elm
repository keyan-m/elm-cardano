module Cardano.Cip25 exposing
    ( AssetMetadata, Cip25
    , File, Image(..), ImageMime, MimeType, PolicyMetadata, Uri, Version(..)
    , assetMetadata, empty, file, insertAsset, label, singleton, withFile
    , stringFromCbor, stringToCbor
    , imageMimeFromString, imageMimeToMimeType, imageMimeToString
    , mimeTypeFromString, mimeTypeToString
    )

{-| CIP-0025 support.

CIP-0025 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025)
a standard format for metadata of minting transactions so that off-chain tools
can associate a set of information with the tokens originating (or updating) in
that transaction.

@docs Cip25, PolicyMetadata, AssetMetadata

@docs File, Image, ImageMime, MimeType, Uri, Version

@docs empty, singleton, insertAsset, assetMetadata, withFile, file, label

@docs stringFromCbor, stringToCbor

@docs imageMimeFromString, imageMimeToMimeType, imageMimeToString
@docs mimeTypeFromString, mimeTypeToString

-}

import Bytes.Comparable exposing (Bytes)
import Bytes.Map as BytesMap exposing (BytesMap)
import Cardano.Metadatum exposing (Metadatum)
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cbor.Decode as D
import Cbor.Encode as E
import Dict exposing (Dict)
import Natural exposing (Natural)


{-| Datatype for modeling CIP-0025.

CIP-0025 metadata is grouped by policy ID, then by asset name. The `version`
field applies to the whole label-721 payload, not to each asset.

-}
type alias Cip25 =
    { version : Version
    , policies : BytesMap PolicyId PolicyMetadata
    }


{-| CIP-0025 metadata label.
-}
label : Natural
label =
    Natural.fromSafeInt 721


{-| Create empty CIP-0025 metadata for a version.
-}
empty : Version -> Cip25
empty version =
    { version = version
    , policies = BytesMap.empty
    }


{-| Create CIP-0025 metadata for one asset.
-}
singleton : Version -> Bytes PolicyId -> Bytes AssetName -> AssetMetadata -> Cip25
singleton version policyId assetName metadata =
    { version = version
    , policies = BytesMap.singleton policyId (BytesMap.singleton assetName metadata)
    }


{-| Insert or replace one asset's metadata.
-}
insertAsset : Bytes PolicyId -> Bytes AssetName -> AssetMetadata -> Cip25 -> Cip25
insertAsset policyId assetName metadata cip25 =
    { cip25
        | policies =
            BytesMap.update policyId
                (\maybePolicyMetadata ->
                    maybePolicyMetadata
                        |> Maybe.withDefault BytesMap.empty
                        |> BytesMap.insert assetName metadata
                        |> Just
                )
                cip25.policies
    }


{-| Metadata for all assets under one policy ID.
-}
type alias PolicyMetadata =
    BytesMap AssetName AssetMetadata


{-| Metadata for a single asset.

The standard simply lays out a set of fields, some of which are optional.

-}
type alias AssetMetadata =
    { name : String
    , image : Image
    , mediaType : Maybe ImageMime
    , description : Maybe String
    , files : List File
    , otherProps : Dict String Metadatum
    }


{-| Create asset metadata with optional fields empty.
-}
assetMetadata : String -> Image -> AssetMetadata
assetMetadata name image =
    { name = name
    , image = image
    , mediaType = Nothing
    , description = Nothing
    , files = []
    , otherProps = Dict.empty
    }


{-| Add a file to asset metadata.
-}
withFile : File -> AssetMetadata -> AssetMetadata
withFile file_ metadata =
    { metadata | files = metadata.files ++ [ file_ ] }


{-| Helper datatype for the `image` field of CIP-0025.

The image is a URI that points to a resource with MIME type `image/*`.
Inline images are represented as `data:` URIs.

TODO: A structured URI model is probably a better design here, so callers can
inspect URI parts before deciding whether to fetch or render an image.

-}
type Image
    = Image Uri


{-| Raw URI string.
-}
type alias Uri =
    String


{-| Datatype to represent standard's version.
-}
type Version
    = V1
    | V2


{-| Datatype to represent optional files specified for an asset.
-}
type alias File =
    { name : String
    , mediaType : MimeType
    , src : Uri
    , otherProps : Dict String Metadatum
    }


{-| Create file metadata with optional fields empty.
-}
file : String -> MimeType -> Uri -> File
file name mediaType src =
    { name = name
    , mediaType = mediaType
    , src = src
    , otherProps = Dict.empty
    }


{-| A MIME media type, such as `image/png` or `application/json`.

The constructor is intentionally opaque so this type can represent future IANA
registrations without hard-coding today's registry into the package.

-}
type MimeType
    = MimeType String


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


{-| A MIME media type constrained to the `image/*` top-level type.

The constructor is intentionally opaque for the same reason as [MimeType]:
image subtypes can be added to the IANA registry over time.

-}
type ImageMime
    = ImageMime String


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


{-| Decode a CIP-0025 string.

CIP-0025 strings are limited to 64 bytes. Longer string values are represented
as a list of 64-byte string chunks.

-}
stringFromCbor : D.Decoder String
stringFromCbor =
    let
        boundedString string =
            if utf8Width string <= 64 then
                D.succeed string

            else
                D.fail
    in
    D.oneOf
        [ D.string |> D.andThen boundedString
        , D.list (D.string |> D.andThen boundedString)
            |> D.map String.concat
        ]


{-| Encode a CIP-0025 string.

Strings longer than 64 bytes are encoded as a list of 64-byte string chunks.

-}
stringToCbor : String -> E.Encoder
stringToCbor string =
    case chunksOfBytes 64 string of
        [] ->
            E.string ""

        [ chunk ] ->
            E.string chunk

        chunks ->
            E.list E.string chunks


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
