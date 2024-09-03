import Window
import Mouse
import Keyboard
import Debug
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time             exposing (..)
{-
I should mention that I know do know better... but I'm following a guide,
so while I know what _not_ to do, I don't have any clear instruction on
what I should be doing instead. So... sorry.
-}

-- MODEL

-- Constants
moveSpeedFactor   = 0.2
bulletSpeedFactor = 10
playerSize        = 20

-- Records
type alias Point =
  { x : Float, y : Float }
type alias Player =
  { x : Float, y : Float, vx : Float, shoot : Bool }
type alias Game =
  { player : Player, bullets : List Point, enemies : List Point }
type alias Keys =
  { x : Int, y : Int }

defPlayer : Player
defPlayer =
  { x = 0, y = 0, vx = 0, shoot = False }

defGame : Game
defGame =
  { player = defPlayer, bullets = [], enemies = [] }


-- UPDATE

update : (Float, Keys) -> Game -> Game
update (dt, keys) game =
  let player' =
        game.player
          |> movePlayer keys
          |> physics dt
          |> shoot keys
      bullets' =
        game.bullets
          |> moveBullets dt
          |> addBullet
              game.player.shoot
              { x = game.player.x, y = game.player.y }
      enemies' =
        game.enemies
  in Debug.watch "game"
    { game | player  <- player'
           , bullets <- bullets'
           , enemies <- enemies'
           }

movePlayer : Keys -> Player -> Player
movePlayer : kets p =
  { p | vx <- toFloat keys.x }

shoot : Keys -> Player -> Player
shoot keys p =
  { p | shoot <- keys.y > 0 }

physics : Float -> Player -> Player
physics dt p =
  { p | x <- p.x + dt*p.vx }

moveBullets : Float -> List Point -> List Point
moveBullets dt List =
  List.map (\b -> { b | y <- b.y + 1*bulletSpeedFactor }) list

addBullet : Bool -> Point -> List Point -> List Point
addBullet really p list =
  if really then p :: list else list


-- VIEW

renderBullet : Float -> Point -> Form
renderBullet yOffset point =
  ngon 3 5
    |> filled Color.red
    |> move (point.x, point.y + yOffset)
    |> rotate (degrees 90)

render : (Int, Int) -> Game -> Element
render (w', h') game =
  let (w,h)   = (toFloat w', toFloat h')
      yOffset = playerSize - (h/2)
      p       = game.player
      player  = ngon 3 playerSize
                  |> filled Color.black
                  |> move (p.x, p.y + yOffset)
                  |> rotate (degrees 90)
      bg      = rect w h
                  |> filled Color.lightGray
      bullets = List.map (renderBullet (yOffset + playerSize))
                  (List.filter (\b -> b.y < h) game.bullets)
      forms   = [ bg, player ] ++ bullets
  in  collage w' h'
        forms

-- SIGNALS

main : Signal Element
main =
  Signal.map2 render Window.dimensions (Signal.foldp update defGame input)

input : Signal (Float, Keys)
input =
  let delta = Signal.map (\t -> t*moveSpeedFactor) (fps 30)
    deltaArrows =
      Signal.map2 (,) delta (Signal.map (Debug.watch "arrows") Keyboard.arrows)
  in
    Signal.sampleOn delta deltaArrows
