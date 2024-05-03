module UnisonShare.UnisonRelease exposing (..)


type alias UnisonRelease =
    { name : String
    , tag : String
    }


latest : UnisonRelease
latest =
    m5


all : List UnisonRelease
all =
    latest :: past


past : List UnisonRelease
past =
    [ m4, m3, m2, m1 ]



-- INSTALL INSTRUCTIONS


installForMac : UnisonRelease -> String
installForMac _ =
    "brew install unisonweb/unison/unison-language"


installForLinux : UnisonRelease -> List String
installForLinux release =
    [ "mkdir unisonlanguage"
    , "curl -L " ++ linuxUrl release ++ " --output unisonlanguage/ucm.tar.gz"
    , "tar -xzf unisonlanguage/ucm.tar.gz -C unisonlanguage"
    , "./unisonlanguage/ucm"
    ]


macUrl : UnisonRelease -> String
macUrl release =
    "https://github.com/unisonweb/unison/releases/download/" ++ release.tag ++ "/ucm-macos.tar.gz"


linuxUrl : UnisonRelease -> String
linuxUrl release =
    "https://github.com/unisonweb/unison/releases/download/" ++ release.tag ++ "/ucm-linux.tar.gz"


windowsUrl : UnisonRelease -> String
windowsUrl release =
    "https://github.com/unisonweb/unison/releases/download/" ++ release.tag ++ "/ucm-windows.zip"



-- RELEASES


m5 : UnisonRelease
m5 =
    { name = "M5", tag = "release%2FM5" }


m4 : UnisonRelease
m4 =
    { name = "M4", tag = "release%2FM4" }


m3 : UnisonRelease
m3 =
    { name = "M3", tag = "release%2FM3" }


m2 : UnisonRelease
m2 =
    { name = "M2", tag = "release%2FM2" }


m1 : UnisonRelease
m1 =
    { name = "M2", tag = "release%2FM1" }
