module UnisonShare.Page.ErrorPage exposing (..)

import Http
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UnisonShare.ErrorCard as ErrorCard
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Session exposing (Session)


view : Session -> Http.Error -> String -> String -> PageLayout msg
view session error entityName className =
    PageLayout.centeredNarrowLayout
        (PageContent.oneColumn
            [ ErrorCard.view
                session
                error
                entityName
                className
            ]
            |> PageContent.withPageTitle (PageTitle.title "Error")
        )
        PageFooter.pageFooter
        |> PageLayout.withSubduedBackground
