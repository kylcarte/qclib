import Graphics.Input as Input
import Graphics.Element as Elem
import Map

passwdOkColour : String -> String -> Element -> Element
passwdOkColour passwd1 passwd2 = 
  if passwd1 == passwd2 && length passwd1 >= 6 
    then Elem.color green 
    else Elem.color red

display : Element -> Element -> Element -> Element
display a b c = a `above` b `above` c

passwdFld : String -> String -> (Signal Element, Signal String)
passwdFld label placeHld = first
  (lift <| beside <| plainText label)
  (Input.password placeHld)

dynamicElement : Signal Element
dynamicElement = let
    (sFld1,sPwd1)   = passwdFld "Password: " "(min. 6 characters)"
    (sFld2,sPwd2)   = passwdFld "Confirm: " "please retype"
    (subBtn,sPress) = Input.button "Submit"
    sColoredBtn     = passwdOkColour <~ sPwd1 ~ sPwd2 ~ constant subBtn
  in
  display <~ sFld1 ~ sFld2 ~ sColoredBtn

-- column : (Maybe Int, [Signal Element]) -> Position -> Signal Element
-- column (mWidth,es) = 

--------------------------------------------------

first : (a -> b) -> (a,c) -> (b,c)
first f (a,b) = (f a, b)

second : (b -> c) -> (a,b) -> (a,c)
second g (a,b) = (a, g b)

main = dynamicElement
