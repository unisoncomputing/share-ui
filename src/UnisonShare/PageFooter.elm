module UnisonShare.PageFooter exposing (..)

import UI.PageLayout exposing (PageFooter(..))
import UnisonShare.Link as Link


pageFooter : PageFooter msg
pageFooter =
    PageFooter
        [ Link.view "Status" Link.status
        , Link.view "Code of Conduct" Link.codeOfConduct
        , Link.view "Terms of Service" Link.termsOfService
        , Link.view "Privacy Policy" Link.privacyPolicy
        ]
