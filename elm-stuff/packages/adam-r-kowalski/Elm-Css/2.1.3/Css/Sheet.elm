module Css.Sheet (createRule, sheetAsString, ruleAsString) where

{-| Create a stylesheet from a list of rules which are a list of Styles
#Get a single name, value pair as string
@docs createRule
@docs sheetAsString
@docs ruleAsString
-}

-- Native Imports
import String exposing (concat)

--My Imports
import Css exposing (Styles)


{-| name, value pair -}
type alias Pair = (String, String)
type alias Rule =
  { name : String
  , styles : Styles
  }

type alias Sheet = List Rule

                 
{-| Create a name, styles pair -}
createRule : String -> Styles -> Rule
createRule name styles =
  { name = name
  , styles = styles
  }


{-| Get string representation of a whole sheet -}
sheetAsString : Sheet -> String
sheetAsString sheet =
  concat <| List.map ruleAsString sheet


{-| Get string representation of a rule -}
ruleAsString : Rule -> String
ruleAsString rule =
  let representation = (concat (List.map (\s -> pairAsString s) rule.styles))
  in rule.name ++ " {\n" ++ representation ++ "\n }\n"


{-| Get string representation of a single pair -}
pairAsString : Pair -> String
pairAsString (name, value) = "  " ++ name ++ ": " ++ value ++ ";\n"
