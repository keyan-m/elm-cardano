module Cardano.Cip25 exposing
    ( Cip25
    , File, Image(..), ImageMime(..), MimeType(..), Version
    )

{-| CIP-0025 support.

CIP-0025 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025)
a standard format for metadata of minting transactions so that off-chain tools
can associate a set of information with the tokens originating (or updating) in
that transaction.

@docs Cip25

@docs File, Image, ImageMime, MimeType, Version

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
    , description : String
    , files : List File
    , version : Version
    , otherProps : Dict String Metadatum
    }


{-| Helper datatype for the `image` field of CIP-0025.

The image can be either a URI that points to a resource with MIME type
`image/*`, or an inline base64-encoded string.

Should this datatype also support inlined SVG strings? e.g.
`data:image/svg+xml,%3Csvg width='45' viewBox=... svg%3E`

-}
type Image
    = ImageUri
        { scheme : String
        , cid : String
        }
    | InlineImage
        { mediaType : ImageMime
        , base64Encoded : String
        }


{-| Datatype to represent a versioning which is compliant with [schema.org](https://schema.org).
-}
type alias Version =
    { primary : Int
    , secondary : Int
    }


{-| Datatype to represent optional files specified for an asset.
-}
type alias File =
    { name : String
    , mediaType : ( MimeType, String )
    , src : String
    , otherProps : Dict String Metadatum
    }


{-| Sum type to represent MIME Content Types listed in [IANA registry](https://iana.org/assignments/media-types/media-types.xhtml).
-}
type MimeType
    = ApplicationMimeType
    | AudioMimeType
    | FontMimeType
    | ExampleMimeType
    | ImageMimeType
    | MessageMimeType
    | ModelMimeType
    | MultipartMimeType
    | TextMimeType
    | VideoMimeType


{-| Dedicated datatype for all image MIME media types according
to [IANA registry](https://iana.org/assignments/media-types/media-types.xhtml#image).

Since the `image` field of CIP-0025 is required, and also must be one of the
image types, this datatype leads to a more robust model with the compromise of
limited support.

TODO: Adding a custom variant (arbitrary string) will allow two representations
for defined constructors, however it seems inevitable in order to support
future image MIMEs. This is also true for [MimeType]'s current implementation.

Also, having this completely decoupled from [MimeType] may not be a great idea.

-}
type ImageMime
    = Bmp
    | Gif
    | Jpeg
    | Png
    | SvgXml
    | Tiff
    | Vnf_adobe_photoshop
    | Vnd_dwg
    | Vnd_dxf
    | Webp
    | Wmf
