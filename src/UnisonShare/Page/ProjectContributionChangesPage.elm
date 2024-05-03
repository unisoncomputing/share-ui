module UnisonShare.Page.ProjectContributionChangesPage exposing (..)

import Code.Definition.Reference as Reference exposing (Reference(..))
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Perspective as Perspective
import Html exposing (Html, div, h2, span, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import List.Nonempty as NEL
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra exposing (pluralize)
import UI
import UI.Card as Card
import UI.Divider as Divider
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.TabList as TabList
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Contribution exposing (Contribution)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.Diff as Diff exposing (Diff)
import UnisonShare.Link as Link
import UnisonShare.Project.ProjectRef exposing (ProjectRef)



-- MODEL


type alias Model =
    { diff : WebData Diff
    }


type alias DiffBranches =
    { oldBranch : Diff.DiffBranchRef
    , newBranch : Diff.DiffBranchRef
    }


init : AppContext -> ProjectRef -> ContributionRef -> ( Model, Cmd Msg )
init appContext projectRef contribRef =
    ( { diff = Loading }, fetchDiff appContext projectRef contribRef )



-- UPDATE


type Msg
    = FetchDiffFinished (WebData Diff)


update : AppContext -> ProjectRef -> ContributionRef -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ msg model =
    case msg of
        FetchDiffFinished contribDiff ->
            ( { model | diff = contribDiff }, Cmd.none )



-- EFFECTS


fetchDiff : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchDiff appContext projectRef contributionRef =
    ShareApi.projectContributionDiff projectRef contributionRef
        |> HttpApi.toRequest Diff.decode (RemoteData.fromResult >> FetchDiffFinished)
        |> HttpApi.perform appContext.api



-- VIEW


branchLink : ProjectRef -> Diff.DiffBranchRef -> Reference -> Html Msg -> Html Msg
branchLink projectRef diffBranchRef ref label =
    Link.projectBranchDefinition
        projectRef
        diffBranchRef.ref
        (Perspective.absoluteRootPerspective diffBranchRef.hash)
        ref
        |> Link.view_ label


viewDiffIcon : Diff.DefinitionDiff -> Html Msg
viewDiffIcon defDiff =
    let
        ( className, icon, tooltipContent ) =
            case defDiff of
                Diff.Added _ ->
                    ( "added", Icon.largePlus, "Added" )

                Diff.Removed _ ->
                    ( "removed", Icon.trash, "Removed" )

                Diff.Updated _ ->
                    ( "updated", Icon.writingPad, "Updated" )

                Diff.RenamedFrom _ ->
                    ( "renamed", Icon.tag, "Renamed" )

                Diff.Aliased _ ->
                    ( "aliased", Icon.tags, "Aliased" )
    in
    Tooltip.text tooltipContent
        |> Tooltip.tooltip
        |> Tooltip.withArrow Tooltip.Start
        |> Tooltip.view (span [ class "diff-icon", class className ] [ Icon.view icon ])


viewDiffLineDefinitionIcon : Diff.DiffLine -> Html msg
viewDiffLineDefinitionIcon diffLine =
    let
        ( description, icon ) =
            case diffLine of
                Diff.TermDiffLine _ ->
                    ( "Term", Icon.term )

                Diff.TypeDiffLine _ ->
                    ( "Type", Icon.type_ )

                Diff.DocDiffLine _ ->
                    ( "Doc", Icon.doc )

                Diff.AbilityDiffLine _ ->
                    ( "Ability", Icon.ability )

                Diff.AbilityConstructorDiffLine _ ->
                    ( "Ability Constructor", Icon.abilityConstructor )

                Diff.DataConstructorDiffLine _ ->
                    ( "Data Constructor", Icon.dataConstructor )

                Diff.TestDiffLine _ ->
                    ( "Test", Icon.test )

                Diff.NamespaceDiffLine _ ->
                    ( "Namespace", Icon.folder )
    in
    div [ class "def-icon-anchor" ]
        [ Tooltip.text description
            |> Tooltip.tooltip
            |> Tooltip.withArrow Tooltip.Start
            |> Tooltip.view (span [ class "def-icon" ] [ Icon.view icon ])
        ]


viewDiffLine : ProjectRef -> DiffBranches -> Diff.DiffLine -> Html Msg
viewDiffLine projectRef diffBranches diffLine =
    let
        sourceBranchLink_ ref label =
            branchLink projectRef diffBranches.newBranch ref label

        targetBranchLink_ ref label =
            branchLink projectRef diffBranches.oldBranch ref label

        viewDefinitionDiff_ refCtor prefix defDiff =
            let
                prefix_ =
                    if String.isEmpty prefix then
                        UI.nothing

                    else
                        span [ class "prefix" ] [ text prefix ]
            in
            case defDiff of
                Diff.Added { hash, shortName, fullName } ->
                    span
                        [ class "diff-info" ]
                        [ prefix_
                        , sourceBranchLink_ (Reference.fromFQN refCtor fullName) (FQN.view shortName)
                        , sourceBranchLink_ (Reference.fromFQN refCtor fullName) (Hash.view hash)
                        ]

                Diff.Removed { hash, shortName, fullName } ->
                    span
                        [ class "diff-info" ]
                        [ prefix_
                        , targetBranchLink_ (Reference.fromFQN refCtor fullName) (FQN.view shortName)
                        , targetBranchLink_ (Reference.fromFQN refCtor fullName) (Hash.view hash)
                        ]

                Diff.Updated { oldHash, newHash, shortName, fullName } ->
                    span
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

                Diff.RenamedFrom { hash, oldNames, newShortName, newFullName } ->
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

                Diff.Aliased { hash, aliasShortName, aliasFullName, otherNames } ->
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
            [ diffIcon
            , defIcon
            , diffContent
            ]

        ( diffLineClass, content ) =
            case diffLine of
                Diff.TermDiffLine d ->
                    ( "term", viewDiffLine_ (viewDiffIcon d) (viewDefinitionDiff_ TermReference "" d) )

                Diff.TypeDiffLine d ->
                    ( "type", viewDiffLine_ (viewDiffIcon d) (viewDefinitionDiff_ TypeReference "type" d) )

                Diff.DocDiffLine d ->
                    ( "doc", viewDiffLine_ (viewDiffIcon d) (viewDefinitionDiff_ TermReference "" d) )

                Diff.AbilityDiffLine d ->
                    ( "ability", viewDiffLine_ (viewDiffIcon d) (viewDefinitionDiff_ TypeReference "ability" d) )

                Diff.AbilityConstructorDiffLine d ->
                    ( "ability-constructor", viewDiffLine_ (viewDiffIcon d) (viewDefinitionDiff_ AbilityConstructorReference "" d) )

                Diff.DataConstructorDiffLine d ->
                    ( "data-constructor", viewDiffLine_ (viewDiffIcon d) (viewDefinitionDiff_ DataConstructorReference "" d) )

                Diff.TestDiffLine d ->
                    ( "test", viewDiffLine_ (viewDiffIcon d) (viewDefinitionDiff_ TermReference "" d) )

                Diff.NamespaceDiffLine ns ->
                    viewNamespaceLine projectRef diffBranches ns
    in
    div [ class "diff-line", class diffLineClass ] content


viewContributionDiffGroup : ProjectRef -> DiffBranches -> List Diff.DiffLine -> Html Msg
viewContributionDiffGroup projectRef diffBranches lines =
    div [ class "contribution-diff-group" ] (List.map (viewDiffLine projectRef diffBranches) lines)


viewNamespaceLine : ProjectRef -> DiffBranches -> { name : FQN.FQN, lines : List Diff.DiffLine } -> ( String, List (Html Msg) )
viewNamespaceLine projectRef diffBranches { name, lines } =
    ( "namespace"
    , [ div [ class "namespace-info" ] [ Icon.view Icon.folder, FQN.view name ]
      , viewContributionDiffGroup projectRef diffBranches lines
      ]
    )


viewDiff : AppContext -> ProjectRef -> Diff -> Html Msg
viewDiff _ projectRef diff =
    let
        summary =
            Diff.summary diff.lines

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
        , viewContributionDiffGroup projectRef diffBranches diff.lines
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
    case model.diff of
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
                , div [ class "project-contribution-changes-page" ] [ viewDiff appContext projectRef diff ]
                ]

        Failure e ->
            viewErrorPage contribution.ref e
