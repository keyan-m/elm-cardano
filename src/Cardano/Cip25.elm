module Cardano.Cip25 exposing
    ( Cip25
    , File, Image(..), ImageMime, MimeType, Uri, Version
    , imageMimeFromString, imageMimeToMimeType, imageMimeToString
    , mimeTypeFromString, mimeTypeToString
    )

{-| CIP-0025 support.

CIP-0025 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025)
a standard format for metadata of minting transactions so that off-chain tools
can associate a set of information with the tokens originating (or updating) in
that transaction.

@docs Cip25

@docs File, Image, ImageMime, MimeType, Uri, Version

@docs imageMimeFromString, imageMimeToMimeType, imageMimeToString
@docs mimeTypeFromString, mimeTypeToString

-}

import Cardano.Metadatum exposing (Metadatum)
import Dict exposing (Dict)


{-| Datatype for modeling CIP-0025.

The standard simply lays out a set of fields, some of which are optional.

-}
type alias Cip25 =
    { name : String
    , image : Image
    , mediaType : Maybe ImageMime
    , description : Maybe String
    , files : List File
    , version : Version
    , otherProps : Dict String Metadatum
    }


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
    case String.toList name of
        [] ->
            False

        first :: rest ->
            String.length name <= 127
                && (((first >= 'A') && (first <= 'Z'))
                        || ((first >= 'a') && (first <= 'z'))
                        || ((first >= '0') && (first <= '9'))
                   )
                && List.all
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
