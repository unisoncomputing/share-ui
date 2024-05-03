module UnisonShare.Project.ProjectRef exposing
    ( ProjectRef
    , decode
    , equals
    , fromString
    , handle
    , projectRef
    , slug
    , toApiStringParts
    , toParts
    , toProjectName
    , toString
    , toStringParts
    , toUrlPath
    , unsafeFromString
    , view
    , viewClickable
    , viewHashvatar
    , view_
    )

import Code.Hash as Hash
import Code.Hashvatar as Hashvatar
import Code.ProjectName as ProjectName exposing (ProjectName)
import Code.ProjectSlug as ProjectSlug exposing (ProjectSlug)
import Html exposing (Html, label, span, text)
import Html.Attributes exposing (class, title)
import Json.Decode as Decode
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util as Util
import UI.Click as Click exposing (Click)


type ProjectRef
    = ProjectRef { handle : UserHandle, slug : ProjectSlug }


fromString : String -> String -> Maybe ProjectRef
fromString rawHandle rawSlug =
    Maybe.map2 projectRef
        (UserHandle.fromString rawHandle)
        (ProjectSlug.fromString rawSlug)


{-| Don't use! It's meant for tests
-}
unsafeFromString : String -> String -> ProjectRef
unsafeFromString rawHandle rawSlug =
    projectRef
        (UserHandle.unsafeFromString rawHandle)
        (ProjectSlug.unsafeFromString rawSlug)


projectRef : UserHandle -> ProjectSlug -> ProjectRef
projectRef handle_ slug_ =
    ProjectRef { handle = handle_, slug = slug_ }


toString : ProjectRef -> String
toString (ProjectRef p) =
    UserHandle.toString p.handle ++ "/" ++ ProjectSlug.toString p.slug


toUrlPath : ProjectRef -> List String
toUrlPath (ProjectRef p) =
    [ UserHandle.toString p.handle, ProjectSlug.toString p.slug ]


toProjectName : ProjectRef -> ProjectName
toProjectName (ProjectRef p) =
    ProjectName.projectName (Just p.handle) p.slug


handle : ProjectRef -> UserHandle
handle (ProjectRef p) =
    p.handle


slug : ProjectRef -> ProjectSlug
slug (ProjectRef p) =
    p.slug


toParts : ProjectRef -> ( UserHandle, ProjectSlug )
toParts (ProjectRef p) =
    ( p.handle, p.slug )


toStringParts : ProjectRef -> ( String, String )
toStringParts (ProjectRef p) =
    ( UserHandle.toString p.handle, ProjectSlug.toString p.slug )


toApiStringParts : ProjectRef -> ( String, String )
toApiStringParts (ProjectRef p) =
    ( UserHandle.toUnprefixedString p.handle, ProjectSlug.toString p.slug )


equals : ProjectRef -> ProjectRef -> Bool
equals a b =
    toString a == toString b


viewHashvatar : ProjectRef -> Html msg
viewHashvatar ref =
    let
        hash =
            Hash.unsafeFromString (toString ref)
    in
    Hashvatar.view hash


view : ProjectRef -> Html msg
view projectRef_ =
    view_ Nothing Nothing projectRef_


viewClickable : (UserHandle -> Click msg) -> (ProjectRef -> Click msg) -> ProjectRef -> Html msg
viewClickable handleClick slugClick projectRef_ =
    view_ (Just handleClick) (Just slugClick) projectRef_


view_ : Maybe (UserHandle -> Click msg) -> Maybe (ProjectRef -> Click msg) -> ProjectRef -> Html msg
view_ handleClick slugClick ((ProjectRef p) as projectRef_) =
    let
        handle_ =
            case handleClick of
                Just c ->
                    Click.view [ class "project-ref_handle" ] [ text (UserHandle.toString p.handle) ] (c p.handle)

                Nothing ->
                    span [ class "project-ref_handle" ] [ text (UserHandle.toString p.handle) ]

        slug_ =
            case slugClick of
                Just c ->
                    Click.view [ class "project-ref_slug" ] [ text (ProjectSlug.toString p.slug) ] (c projectRef_)

                Nothing ->
                    span [ class "project-ref_slug" ] [ text (ProjectSlug.toString p.slug) ]
    in
    label [ class "project-ref", title (toString projectRef_) ]
        [ handle_
        , span [ class "project-ref_separator" ] [ text "/" ]
        , slug_
        ]



-- DECODE


decode : Decode.Decoder ProjectRef
decode =
    let
        fromString_ s =
            case String.split "/" s of
                [ handle_, slug_ ] ->
                    fromString handle_ slug_

                _ ->
                    Nothing
    in
    Decode.map fromString_ Decode.string
        |> Decode.andThen (Util.decodeFailInvalid "Invalid ProjectRef")
