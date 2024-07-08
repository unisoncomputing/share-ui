module UnisonShare.Page.ProjectContributionChangesPage exposing (..)

import Code.Definition.Reference as Reference exposing (Reference(..))
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Perspective as Perspective
import Html exposing (Html, code, div, h2, pre, span, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import List.Nonempty as NEL
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra exposing (pluralize)
import UI
import UI.Card as Card
import UI.Click as Click
import UI.Divider as Divider
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.TabList as TabList
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchDiff as BranchDiff exposing (BranchDiff)
import UnisonShare.Contribution exposing (Contribution)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.DefinitionDiff as DefinitionDiff exposing (DefinitionDiff)
import UnisonShare.DefinitionDiffKey as DefinitionDiffKey exposing (DefinitionDiffKey)
import UnisonShare.DefinitionDiffs as DefinitionDiffs exposing (DefinitionDiffs)
import UnisonShare.Link as Link
import UnisonShare.Project.ProjectRef exposing (ProjectRef)



-- MODEL


type alias Model =
    { branchDiff : WebData BranchDiff
    , definitionDiffs : DefinitionDiffs
    }


type alias DiffBranches =
    { oldBranch : BranchDiff.DiffBranchRef
    , newBranch : BranchDiff.DiffBranchRef
    }


init : AppContext -> ProjectRef -> ContributionRef -> ( Model, Cmd Msg )
init appContext projectRef contribRef =
    ( { branchDiff = Loading
      , definitionDiffs = DefinitionDiffs.empty
      }
    , fetchBranchDiff appContext projectRef contribRef
    )



-- UPDATE


type Msg
    = FetchBranchDiffFinished (WebData BranchDiff)
    | FetchDefinitionDiff DefinitionDiffKey
    | FetchDefinitionDiffFinished DefinitionDiffKey (WebData DefinitionDiff)


update : AppContext -> ProjectRef -> ContributionRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef _ msg model =
    case msg of
        FetchBranchDiffFinished branchDiff ->
            ( { model | branchDiff = branchDiff }, Cmd.none )

        FetchDefinitionDiff key ->
            let
                definitionDiffs =
                    DefinitionDiffs.set model.definitionDiffs key Loading
            in
            ( { model | definitionDiffs = definitionDiffs }, fetchDefinitionDiff appContext projectRef key )

        FetchDefinitionDiffFinished key resp ->
            let
                definitionDiffs =
                    DefinitionDiffs.set model.definitionDiffs key resp
            in
            ( { model | definitionDiffs = definitionDiffs }, Cmd.none )



-- EFFECTS


fetchBranchDiff : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchBranchDiff appContext projectRef contributionRef =
    ShareApi.projectContributionDiff projectRef contributionRef
        |> HttpApi.toRequest BranchDiff.decode (RemoteData.fromResult >> FetchBranchDiffFinished)
        |> HttpApi.perform appContext.api


fetchDefinitionDiff : AppContext -> ProjectRef -> DefinitionDiffKey -> Cmd Msg
fetchDefinitionDiff appContext projectRef key =
    let
        definitionType =
            case key of
                DefinitionDiffKey.Term _ ->
                    DefinitionDiff.Term

                DefinitionDiffKey.Type _ ->
                    DefinitionDiff.Type
    in
    ShareApi.projectBranchDefinitionDiff projectRef key
        |> HttpApi.toRequest
            (DefinitionDiff.decode definitionType)
            (RemoteData.fromResult >> FetchDefinitionDiffFinished key)
        |> HttpApi.perform appContext.api



-- VIEW


branchLink : ProjectRef -> BranchDiff.DiffBranchRef -> Reference -> Html Msg -> Html Msg
branchLink projectRef diffBranchRef ref label =
    Link.projectBranchDefinition
        projectRef
        diffBranchRef.ref
        (Perspective.absoluteRootPerspective diffBranchRef.hash)
        ref
        |> Link.view_ label


viewDiffIcon : BranchDiff.DiffLineItem -> Html Msg
viewDiffIcon defDiff =
    let
        ( className, icon, tooltipContent ) =
            case defDiff of
                BranchDiff.Added _ ->
                    ( "added", Icon.largePlus, "Added" )

                BranchDiff.Removed _ ->
                    ( "removed", Icon.trash, "Removed" )

                BranchDiff.Updated _ ->
                    ( "updated", Icon.writingPad, "Updated" )

                BranchDiff.RenamedFrom _ ->
                    ( "renamed", Icon.tag, "Renamed" )

                BranchDiff.Aliased _ ->
                    ( "aliased", Icon.tags, "Aliased" )
    in
    Tooltip.text tooltipContent
        |> Tooltip.tooltip
        |> Tooltip.withArrow Tooltip.Start
        |> Tooltip.view (span [ class "diff-icon", class className ] [ Icon.view icon ])


viewDiffLineDefinitionIcon : BranchDiff.DiffLine -> Html msg
viewDiffLineDefinitionIcon diffLine =
    let
        ( description, icon ) =
            case diffLine of
                BranchDiff.TermDiffLine _ ->
                    ( "Term", Icon.term )

                BranchDiff.TypeDiffLine _ ->
                    ( "Type", Icon.type_ )

                BranchDiff.DocDiffLine _ ->
                    ( "Doc", Icon.doc )

                BranchDiff.AbilityDiffLine _ ->
                    ( "Ability", Icon.ability )

                BranchDiff.AbilityConstructorDiffLine _ ->
                    ( "Ability Constructor", Icon.abilityConstructor )

                BranchDiff.DataConstructorDiffLine _ ->
                    ( "Data Constructor", Icon.dataConstructor )

                BranchDiff.TestDiffLine _ ->
                    ( "Test", Icon.test )

                BranchDiff.NamespaceDiffLine _ ->
                    ( "Namespace", Icon.folder )
    in
    div [ class "def-icon-anchor" ]
        [ Tooltip.text description
            |> Tooltip.tooltip
            |> Tooltip.withArrow Tooltip.Start
            |> Tooltip.view (span [ class "def-icon" ] [ Icon.view icon ])
        ]


viewDiffLine : ProjectRef -> DefinitionDiffs -> DiffBranches -> BranchDiff.DiffLine -> Html Msg
viewDiffLine projectRef definitionDiffs diffBranches diffLine =
    let
        sourceBranchLink_ ref label =
            branchLink projectRef diffBranches.newBranch ref label

        targetBranchLink_ ref label =
            branchLink projectRef diffBranches.oldBranch ref label

        termKey defA defB =
            DefinitionDiffKey.Term
                { branchA = diffBranches.newBranch.ref
                , branchB = diffBranches.oldBranch.ref
                , definitionA = defA
                , definitionB = defB
                }

        typeKey defA defB =
            DefinitionDiffKey.Type
                { branchA = diffBranches.newBranch.ref
                , branchB = diffBranches.oldBranch.ref
                , definitionA = defA
                , definitionB = defB
                }

        viewDiffLineItem refCtor mkKey prefix lineItem =
            let
                prefix_ =
                    if String.isEmpty prefix then
                        UI.nothing

                    else
                        span [ class "prefix" ] [ text prefix ]
            in
            case lineItem of
                BranchDiff.Added { hash, shortName, fullName } ->
                    span
                        [ class "diff-info" ]
                        [ prefix_
                        , sourceBranchLink_ (Reference.fromFQN refCtor fullName) (FQN.view shortName)
                        , sourceBranchLink_ (Reference.fromFQN refCtor fullName) (Hash.view hash)
                        ]

                BranchDiff.Removed { hash, shortName, fullName } ->
                    span
                        [ class "diff-info" ]
                        [ prefix_
                        , targetBranchLink_ (Reference.fromFQN refCtor fullName) (FQN.view shortName)
                        , targetBranchLink_ (Reference.fromFQN refCtor fullName) (Hash.view hash)
                        ]

                BranchDiff.Updated { oldHash, newHash, shortName, fullName } ->
                    let
                        key : DefinitionDiffKey
                        key =
                            mkKey fullName fullName
                    in
                    div []
                        [ Click.view
                            [ class "diff-info" ]
                            [ prefix_
                            , sourceBranchLink_ (Reference.fromFQN refCtor fullName) (FQN.view shortName)
                            , sourceBranchLink_ (Reference.fromFQN refCtor fullName) (Hash.view newHash)
                            , span [ class "extra-info" ]
                                [ text " (updated from "
                                , targetBranchLink_ (Reference.fromFQN refCtor fullName) (Hash.view oldHash)
                                , text ")"
                                ]
                            ]
                            (Click.onClick (FetchDefinitionDiff key))
                        , DefinitionDiffs.get definitionDiffs key
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map (\d -> pre [] [ code [] [ DefinitionDiff.view d ] ])
                            |> Maybe.withDefault UI.nothing
                        ]

                BranchDiff.RenamedFrom { hash, oldNames, newShortName, newFullName } ->
                    span [ class "diff-info" ]
                        [ prefix_
                        , sourceBranchLink_ (Reference.fromFQN refCtor newFullName) (FQN.view newShortName)
                        , sourceBranchLink_ (Reference.fromFQN refCtor newFullName) (Hash.view hash)
                        , span [ class "extra-info" ]
                            (text "(was "
                                :: (oldNames
                                        |> NEL.map
                                            (\fqn -> targetBranchLink_ (Reference.fromFQN refCtor fqn) (FQN.view fqn))
                                        |> NEL.toList
                                        |> List.intersperse (text ", ")
                                   )
                                ++ [ text ")" ]
                            )
                        ]

                BranchDiff.Aliased { hash, aliasShortName, aliasFullName, otherNames } ->
                    span [ class "diff-info" ]
                        [ prefix_
                        , sourceBranchLink_ (Reference.fromFQN refCtor aliasFullName) (FQN.view aliasShortName)
                        , sourceBranchLink_ (Reference.fromFQN refCtor aliasFullName) (Hash.view hash)
                        , span [ class "extra-info" ]
                            (text "(AKA "
                                :: (otherNames
                                        |> NEL.map
                                            (\fqn -> sourceBranchLink_ (Reference.fromFQN refCtor fqn) (FQN.view fqn))
                                        |> NEL.toList
                                        |> List.intersperse (text ", ")
                                   )
                                ++ [ text ")" ]
                            )
                        ]

        defIcon =
            viewDiffLineDefinitionIcon diffLine

        viewDiffLine_ diffIcon diffContent =
            [ diffIcon, defIcon, diffContent ]

        ( diffLineClass, content ) =
            case diffLine of
                BranchDiff.TermDiffLine d ->
                    ( "term", viewDiffLine_ (viewDiffIcon d) (viewDiffLineItem TermReference termKey "" d) )

                BranchDiff.TypeDiffLine d ->
                    ( "type", viewDiffLine_ (viewDiffIcon d) (viewDiffLineItem TypeReference typeKey "type" d) )

                BranchDiff.DocDiffLine d ->
                    ( "doc", viewDiffLine_ (viewDiffIcon d) (viewDiffLineItem TermReference termKey "" d) )

                BranchDiff.AbilityDiffLine d ->
                    ( "ability", viewDiffLine_ (viewDiffIcon d) (viewDiffLineItem TypeReference typeKey "ability" d) )

                BranchDiff.AbilityConstructorDiffLine d ->
                    ( "ability-constructor", viewDiffLine_ (viewDiffIcon d) (viewDiffLineItem AbilityConstructorReference termKey "" d) )

                BranchDiff.DataConstructorDiffLine d ->
                    ( "data-constructor", viewDiffLine_ (viewDiffIcon d) (viewDiffLineItem DataConstructorReference termKey "" d) )

                BranchDiff.TestDiffLine d ->
                    ( "test", viewDiffLine_ (viewDiffIcon d) (viewDiffLineItem TermReference termKey "" d) )

                BranchDiff.NamespaceDiffLine ns ->
                    viewNamespaceLine projectRef definitionDiffs diffBranches ns
    in
    div [ class "diff-line", class diffLineClass ] content


viewContributionDiffGroup : ProjectRef -> DefinitionDiffs -> DiffBranches -> List BranchDiff.DiffLine -> Html Msg
viewContributionDiffGroup projectRef definitionDiffs diffBranches lines =
    div [ class "contribution-diff-group" ] (List.map (viewDiffLine projectRef definitionDiffs diffBranches) lines)


viewNamespaceLine : ProjectRef -> DefinitionDiffs -> DiffBranches -> { name : FQN.FQN, lines : List BranchDiff.DiffLine } -> ( String, List (Html Msg) )
viewNamespaceLine projectRef definitionDiffs diffBranches { name, lines } =
    ( "namespace"
    , [ div [ class "namespace-info" ] [ Icon.view Icon.folder, FQN.view name ]
      , viewContributionDiffGroup projectRef definitionDiffs diffBranches lines
      ]
    )


viewBranchDiff : AppContext -> ProjectRef -> DefinitionDiffs -> BranchDiff -> Html Msg
viewBranchDiff _ projectRef definitionDiffs diff =
    let
        summary =
            BranchDiff.summary diff.lines

        diffBranches =
            { oldBranch = diff.oldBranch
            , newBranch = diff.newBranch
            }
    in
    Card.card
        [ h2 []
            [ text (pluralize "change" "changes" summary.numChanges)
            , text " across "
            , text (pluralize "namespace" "namespaces" summary.numNamespaceChanges)
            ]
        , Divider.divider |> Divider.small |> Divider.withoutMargin |> Divider.view
        , viewContributionDiffGroup projectRef definitionDiffs diffBranches diff.lines
        ]
        |> Card.withClassName "changes"
        |> Card.asContained
        |> Card.view


viewLoadingPage : PageContent Msg
viewLoadingPage =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view
    in
    PageContent.oneColumn
        [ div []
            [ Card.card
                [ shape Placeholder.Large
                , shape Placeholder.Small
                , shape Placeholder.Medium
                ]
                |> Card.asContained
                |> Card.view
            ]
        ]


viewErrorPage : ContributionRef -> Http.Error -> PageContent Msg
viewErrorPage _ _ =
    PageContent.oneColumn
        [ StatusBanner.bad "Something broke on our end and we couldn't show the contribution changes. Please try again."
        ]


view : AppContext -> ProjectRef -> Contribution -> Model -> PageContent Msg
view appContext projectRef contribution model =
    case model.branchDiff of
        NotAsked ->
            viewLoadingPage

        Loading ->
            viewLoadingPage

        Success diff ->
            PageContent.oneColumn
                [ TabList.tabList
                    [ TabList.tab "Overview" (Link.projectContribution projectRef contribution.ref)
                    ]
                    (TabList.tab "Changes" (Link.projectContributionChanges projectRef contribution.ref))
                    []
                    |> TabList.view
                , div [ class "project-contribution-changes-page" ]
                    [ viewBranchDiff appContext
                        projectRef
                        model.definitionDiffs
                        diff
                    ]
                ]

        Failure e ->
            viewErrorPage contribution.ref e
