module UnisonShare.Project.ProjectListing exposing (..)

import Code.ProjectNameListing as ProjectNameListing exposing (ProjectNameListing)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Lib.UserHandle exposing (UserHandle)
import UI
import UI.Click exposing (Click)
import UI.Icon as Icon
import UnisonShare.Project as Project exposing (Project)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)


type ProjectListingColor
    = FullColor
    | Subdued
    | Monochrome


type alias ProjectListing p msg =
    { project : Project p
    , listing : ProjectNameListing msg
    , color : ProjectListingColor
    }


projectListing : Project p -> ProjectListing p msg
projectListing project =
    { project = project
    , listing =
        ProjectNameListing.projectNameListing
            (ProjectRef.toProjectName project.ref)
    , color = FullColor
    }



-- MODIFY


verySubdued : ProjectListing p msg -> ProjectListing p msg
verySubdued pl =
    { pl | listing = ProjectNameListing.verySubdued pl.listing }


small : ProjectListing p msg -> ProjectListing p msg
small pl =
    { pl | listing = ProjectNameListing.small pl.listing }


medium : ProjectListing p msg -> ProjectListing p msg
medium pl =
    { pl | listing = ProjectNameListing.medium pl.listing }


large : ProjectListing p msg -> ProjectListing p msg
large pl =
    { pl | listing = ProjectNameListing.large pl.listing }


huge : ProjectListing p msg -> ProjectListing p msg
huge pl =
    { pl | listing = ProjectNameListing.huge pl.listing }


withColor : ProjectListingColor -> ProjectListing p msg -> ProjectListing p msg
withColor color pl =
    { pl | color = color }


fullColor : ProjectListing p msg -> ProjectListing p msg
fullColor pl =
    { pl | color = FullColor }



{-
   subdued : ProjectListing p msg -> ProjectListing p msg
   subdued pl =
       { pl | color = Subdued }

-}


monochrome : ProjectListing p msg -> ProjectListing p msg
monochrome pl =
    { pl | color = Monochrome }


withClick : (UserHandle -> Click msg) -> (ProjectRef -> Click msg) -> ProjectListing p msg -> ProjectListing p msg
withClick handleClick projectRefClick p =
    let
        slugClick _ =
            projectRefClick p.project.ref

        listing =
            p.listing
                |> ProjectNameListing.withClick handleClick slugClick
    in
    { p | listing = listing }



-- VIEW
-- TODO, Add visibility thingy!


view : ProjectListing p msg -> Html msg
view pl =
    let
        colorClass =
            case pl.color of
                FullColor ->
                    "project-listing_full-color"

                Subdued ->
                    "project-listing_subdued"

                Monochrome ->
                    "project-listing_monochrome"

        visibilityIcon =
            case pl.project.visibility of
                Project.Public ->
                    UI.nothing

                Project.Private ->
                    div [ class "private-icon" ] [ Icon.view Icon.eyeSlash ]
    in
    div [ class "project-listing", class colorClass ]
        [ ProjectNameListing.view pl.listing, visibilityIcon ]
