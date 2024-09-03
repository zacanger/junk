{-
redoing the flickr example thing
btw is there any actual convention for the name of this file? i've seen 'main,
but also 'app' sometimes... never 'index', which to me would seem like the
obvious choice....
-}

import Html                    exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events             exposing (..)
import Http
import Json.Decode     as Json exposing ((:=))
import String
import Task                    exposing (..)
import Window


-- VIEW

view : Int -> String -> String -> Html
view h string imgUrl =
  div [ style (imgStyle h imgUrl) ]
    [ input
      [ placeholder "search"
      , Attr.value string
      , on "input" targetValue (Signal.message query.address)
      , style myStyle
      ]
      []
    ]

myStyle : List (String, String)
myStyle =
  [ ("width", "100%")
  , ("height", "30px")
  , ("padding", "10px 0")
  , ("font-size", "1.6em")
  , ("text-align", "center")
  , ("font-family", "monospace")
  ]

imgStyle : Int -> String -> List (String, String)
imgStyle h src =
  [ ("width", "100%")
  , ("height", toString h ++ "px")
  , ("background-image", "url('" ++ src ++ "')")
  , ("background-position", "center")
  , ("background-repeat", "no-repeat")
  , ("background-attachment", "fixed")
  ]


-- them tubes

main : Signal Html
main =
  Signal.map3 view Window.height query.signal results.signal

results : Signal.Mailbox String
results =
  Signal.mailbox "waiting.gif"

port requestImgs : Signal (Task Http.Error ())
port requestImgs =
  |> sample getImage Window.dimensions
  |> Signal.map (\task -> `andThen` Signal.send results.address)

sample f sampled events =
  Signal.sampleOn events (Signal.map2 f sampled events)

query : Signal.Mailbox String
query =
  Signal.mailbox ""

getImage : (Int,Int) -> String -> Task Http.Error String
getImage dimensions tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in
    Http.get photoList (flickr "search" searchArgs)
      `andThen`
        selectPhoto
      `andThen` \photo ->
        Http.get sizeList (flickr "getSizes" [ ("photo_id", photo.id) ])
      `andThen`
        pickSize.dimensions

type alias Photo =
  { id    : String
  , title : String
  }

type alias Size =
  { source : String
  , width  : Int
  , height : Int
  }

photoList : Json.Decoder (List Photo)
photoList =
  Json.at ["photos", "photo"] <| Json.list <|
    Json.object2 Photo
      ("id"    := Json.string)
      ("title" := Json.string)

sizeList : Json.Decoder (List Size)
sizeList =
  let number =
        Json.oneOf [ Json.int, Json.customDecoder Json.stringString.toInt ]
  in
    Json.at ["sizes", "size"] <| Json.list <|
      ("source" := Json.string)
      ("width"  := number)
      ("height" := number)

-- flickr-side

flickr : String -> List (String, String) -> String
flickr method args =
  Http.url "https://api.flickr.com/services/rest/" <|
    [ ("format", "json")
    , ("nojsoncallback", "1")
    , ("method", "flickr.photos." ++ method)
    , ("api_key", "9be5b08cd8168fa82d136aa55f1fdb3c")
    ] ++ args

selectPhoto : List Photo -> Task Http.Error Photo
selectPhoto photos =
  case photos of
    photo :: _ -> succeed photo
    [] ->
      fail (Http.UnexpectedPayload "was hoping for 1 or more photos from flickr....")

pickSize : (Int, Int) -> List Size -> Task Http.Error String
pickSize (width,height) size =
  let sizeRating size =
    let penalty =
      if size.width > width || size.height > height then 400 else 0
    in
       abs (width - size.width) + abs (height - size.height) + penalty
  in
     case List.sortedBy sizeRating sizes of
       size :: -> succeed size.source
       [] ->
         fail (Http.UnexpactedPayload "nope, now we can, but only maybe... sorry.")
