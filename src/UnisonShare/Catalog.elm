module UnisonShare.Catalog exposing (..)

import Json.Decode as Decode exposing (field, string)
import OrderedDict exposing (OrderedDict)
import UnisonShare.Project as Project exposing (ProjectSummary)
import UnisonShare.Project.ProjectRef as ProjectRef


type Catalog
    = Catalog (OrderedDict String (List ProjectSummary))


type alias CatalogWithFeatured =
    { featured : Maybe (List ProjectSummary)
    , rest : List ( String, List ProjectSummary )
    }



-- HELPERS


{-| Create a Catalog from a list of grouped projects
-}
fromList : List ( String, List ProjectSummary ) -> Catalog
fromList groups =
    Catalog (OrderedDict.fromList groups)


{-| Extract all categories from a Catalog
-}
categories : Catalog -> List String
categories (Catalog dict) =
    OrderedDict.keys dict


{-| Extract all project listings from a Catalog
-}
projectListings : Catalog -> List ProjectSummary
projectListings (Catalog dict) =
    List.concat (OrderedDict.values dict)


{-| Convert a Catalog to a list of project listings grouped by category
-}
toList : Catalog -> List ( String, List ProjectSummary )
toList (Catalog dict) =
    OrderedDict.toList dict


{-| Separate the "Featured" category out from the rest of the catalog entries
-}
asFeatured : Catalog -> CatalogWithFeatured
asFeatured catalog =
    let
        featured__ ( category, projects ) ( f, categories_ ) =
            if String.toLower category == "featured" then
                ( Just projects, categories_ )

            else
                ( f, categories_ ++ [ ( category, projects ) ] )

        ( featured_, rest ) =
            catalog
                |> toList
                |> List.foldl featured__ ( Nothing, [] )
    in
    { featured = featured_, rest = rest }



-- DECODE


decode : Decode.Decoder Catalog
decode =
    let
        decodeGroup =
            Decode.map2 Tuple.pair
                (field "name" string)
                (field "projects"
                    (Decode.map
                        (List.sortBy (.ref >> ProjectRef.toString))
                        (Decode.list Project.decodeSummary)
                    )
                )
    in
    Decode.map fromList (Decode.list decodeGroup)
