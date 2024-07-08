--- A small stateful wrapper around Code.Definition.Doc


module UnisonShare.InteractiveDoc exposing (..)

import Code.Config exposing (Config)
import Code.Definition.Doc as Doc exposing (Doc, DocFoldToggles)
import Code.Definition.Reference exposing (Reference)
import Code.DefinitionSummaryTooltip as DefinitionSummaryTooltip
import Code.Syntax.Linked as Linked
import Html exposing (Html)
import UI.Click as Click



-- MODEL


type alias Model =
    { foldToggles : DocFoldToggles
    , definitionSummaryTooltip : DefinitionSummaryTooltip.Model
    }


init : Model
init =
    { foldToggles = Doc.emptyDocFoldToggles
    , definitionSummaryTooltip = DefinitionSummaryTooltip.init
    }



-- UPDATE


type Msg
    = OpenReference Reference
    | ToggleDocFold Doc.FoldId
    | DefinitionSummaryTooltipMsg DefinitionSummaryTooltip.Msg


type OutMsg
    = OpenDefinition Reference
    | None


update : Config -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update config msg model =
    case msg of
        OpenReference r ->
            ( model, Cmd.none, OpenDefinition r )

        ToggleDocFold fid ->
            ( { model | foldToggles = Doc.toggleFold model.foldToggles fid }, Cmd.none, None )

        DefinitionSummaryTooltipMsg tMsg ->
            let
                ( definitionSummaryTooltip, tCmd ) =
                    DefinitionSummaryTooltip.update config tMsg model.definitionSummaryTooltip
            in
            ( { model | definitionSummaryTooltip = definitionSummaryTooltip }
            , Cmd.map DefinitionSummaryTooltipMsg tCmd
            , None
            )



-- VIEW


view : Model -> Doc -> Html Msg
view model doc =
    let
        syntaxConfig =
            Linked.linkedWithTooltipConfig
                (OpenReference >> Click.onClick)
                (DefinitionSummaryTooltip.tooltipConfig
                    DefinitionSummaryTooltipMsg
                    model.definitionSummaryTooltip
                )
    in
    Doc.view syntaxConfig ToggleDocFold model.foldToggles doc
