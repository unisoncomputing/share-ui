module UnisonShare.User exposing
    ( User
    , UserDetails
    , UserSummary
    , decodeDetails
    , decodeSummary
    , name
    , toAvatar
    )

import Json.Decode as Decode exposing (field, maybe, nullable, string)
import Json.Decode.Pipeline exposing (required)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util exposing (decodeUrl)
import UI.Avatar as Avatar exposing (Avatar)
import UI.Icon as Icon
import Url exposing (Url)


type alias User u =
    { u
        | handle : UserHandle
        , name : Maybe String
        , avatarUrl : Maybe Url
    }


type alias UserSummary =
    User { pronouns : Maybe String }


type alias UserDetails =
    User
        { website : Maybe Url
        , location : Maybe String
        , bio : Maybe String
        , pronouns : Maybe String
        , twitterHandle : Maybe String
        }



-- HELPERS


name : User u -> String
name user =
    Maybe.withDefault (UserHandle.toString user.handle) user.name


toAvatar : User u -> Avatar msg
toAvatar user =
    Avatar.avatar user.avatarUrl (Just (name user))
        |> Avatar.withIcon Icon.user



-- DECODE


decodeSummary : Decode.Decoder UserSummary
decodeSummary =
    let
        makeSummary handle name_ avatarUrl =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            , pronouns = Nothing
            }
    in
    Decode.map3 makeSummary
        (field "handle" UserHandle.decodeUnprefixed)
        (maybe (field "name" string))
        (maybe (field "avatarUrl" decodeUrl))


decodeDetails : Decode.Decoder UserDetails
decodeDetails =
    let
        makeDetails handle name_ avatarUrl pronouns bio location website twitterHandle =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            , pronouns = pronouns
            , bio = bio
            , location = location
            , website = website
            , twitterHandle = twitterHandle
            }
    in
    Decode.succeed makeDetails
        |> required "handle" UserHandle.decodeUnprefixed
        |> required "name" (nullable string)
        |> required "avatarUrl" (nullable decodeUrl)
        |> required "pronouns" (nullable string)
        |> required "bio" (nullable string)
        |> required "location" (nullable string)
        |> required "website" (nullable decodeUrl)
        |> required "twitterHandle" (nullable string)
