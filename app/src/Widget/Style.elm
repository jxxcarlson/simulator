module Widget.Style exposing (darkGray, darkRed, lightRed, mediumGray, myFocusStyle, pureWhite, white)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


gray g =
    Element.rgb g g g


darkRed =
    Element.rgb 0.45 0 0


lightRed =
    Element.rgb 0.7 0 0


white =
    gray 0.9


pureWhite =
    gray 1.0


mediumGray =
    gray 0.6


darkGray =
    gray 0.4


myFocusStyle : Element.FocusStyle
myFocusStyle =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }
