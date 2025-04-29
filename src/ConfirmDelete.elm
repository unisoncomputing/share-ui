module ConfirmDelete exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import UI.Button as Button
import UI.Icon as Icon
import UI.StatusBanner as StatusBanner


type ConfirmDelete
    = Confirm
    | Deleting
    | Deleted
    | DeleteFailed Http.Error


type alias ConfirmDeletes =
    Dict String ConfirmDelete


type alias ViewConfig msg =
    { confirmMsg : msg, cancelMsg : msg }


confirm : ConfirmDelete
confirm =
    Confirm


deleting : ConfirmDelete
deleting =
    Deleting


deleted : ConfirmDelete
deleted =
    Deleted


deleteFailed : Http.Error -> ConfirmDelete
deleteFailed e =
    DeleteFailed e


emptyDeletes : ConfirmDeletes
emptyDeletes =
    Dict.empty


get : String -> ConfirmDeletes -> Maybe ConfirmDelete
get key deletes =
    Dict.get key deletes


set : String -> ConfirmDelete -> ConfirmDeletes -> ConfirmDeletes
set key delete deletes =
    Dict.insert key delete deletes


remove : String -> ConfirmDeletes -> ConfirmDeletes
remove key deletes =
    Dict.remove key deletes


update : String -> (Maybe ConfirmDelete -> Maybe ConfirmDelete) -> ConfirmDeletes -> ConfirmDeletes
update key f deletes =
    Dict.update key f deletes


view : ViewConfig msg -> ConfirmDelete -> Html msg
view { confirmMsg, cancelMsg } confirmDelete =
    let
        view_ content =
            div [ class "confirm-delete" ] content
    in
    case confirmDelete of
        Confirm ->
            view_
                [ text "Are you sure?"
                , Button.iconThenLabel confirmMsg Icon.trash "Yes, delete" |> Button.critical |> Button.small |> Button.view
                , Button.button cancelMsg "Cancel" |> Button.subdued |> Button.small |> Button.view
                ]

        Deleting ->
            view_ [ StatusBanner.working "Deleting..." ]

        Deleted ->
            view_ [ StatusBanner.good "Deleted" ]

        DeleteFailed _ ->
            view_
                [ StatusBanner.bad "Failed to delete"
                , Button.iconThenLabel confirmMsg Icon.trash "Try again" |> Button.critical |> Button.small |> Button.view
                , Button.button cancelMsg "Cancel" |> Button.subdued |> Button.small |> Button.view
                ]
