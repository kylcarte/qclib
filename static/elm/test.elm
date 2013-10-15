
import Http
import Graphics.Input as Input
import Char
import Maybe

(field,rawInput) = Input.field "Zip Code"

toUrl s = if validZip s
  then Just <| "http://zip.elevenbasetwo.com/v2/US/" ++ s
  else Nothing

validZip s = length s == 5 && all Char.isDigit s

realInput = toUrl <~ rawInput

responses = Http.sendGet (fromMaybe "" <~ realInput)

display resp = case resp of
  Http.Success addr -> text . monospace <| toText addr
  Http.Waiting      -> image 16 16 "waiting.gif"
  Http.Failure _ _  -> asText resp

message =
  let msg = plainText "Enter a valid zip code, such as 12345 or 90210."
      output inp rsp = Maybe.maybe msg (\_ -> display rsp) inp
  in output <~ realInput ~ responses

fromMaybe : a -> Maybe a -> a
fromMaybe a m = case m of
  Nothing -> a
  Just b  -> b

main = lift2 above field message

