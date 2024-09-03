module BrickBreaker where

-- skel

import List exposing (map, map2)
import Graphics.Element exposing (show, Element)
import Keyboard
import Text
import Time exposing (Time, fps)
import Signal exposing (Signal, foldp)
import Signal
import Window

-- model

direction : Signal Int
direction = Signal.map .x Keyboard.arrows

type alias Input = {dir: Int, delta: Time}

input : Signal Input
input = Signal.map2 Input direction (fps 60)

type alias Positioned a = {a | x:Float}

type alias Player = Positioned {}

player : Float -> Player
player x = {x=x}

type alias Game = {
                  state:State
                , gameBall:Ball
                , player:Player
                , bricks:List Brick
                , sparBalls:Int
                , contacts:Int }

defaultGame : Game
defaultGame = {player = player 0}

displayFullScreen : (Int, Int) -> Form -> Element
displayFullScreen (w, h) content =
  let
    gameScale = min (toFLoat w/ gameWidth) (toFloat h/ gameHeight)
  in
    collage w h [content |> scale gameScale]

-- updates

stepGame : Input -> Game -> Game
stepGame ({dir, delta} as input) ({state, player} as game) =
  let
    func = if state == Play then stepPlay
      else if state == Serve then stepServe
      else stepGameOver
  in
    func input {game | player = stepPlayer delta dir player}

stepPlay: Input -> Game -> Game
stepPlay {delta} ({gameBall, player, bricks, spareBalls, contacts} as game) =
  let
    ballLost = gameBall.y - gameBall.r < - halfHeight
    gameOver = ballLost && spareBalls = 0
    spareBalls' = if ballLost then Spareballs -1 else spareBalls
    state' = if gameOver then Lost
      else if ballLost then Serve
      else if List.isEmpty bricks then Won
      else Play
    ((ball', bricks'), contacts') =
      stepBall delta gameBall player bricks contacts
    in
      {game | state      = state'
            , gameBall   = ball'
            , bricks     = bricks'
            , spareBalls = max 0 spareBalls'
            , contacts   = contacts' }

stepBall: Time.Time -> Ball -> Player -> List Brick -> Int -> ((Ball, List Brick), Int)
stepBall t ({x, y, vx, vy} as ball) p bricks contacts =
  let
    hitPlayer = (ball `within` p)
    contacts' = if hitPlayer then contacts + 1 else contacts
    newVx = if hitPlayer then
      weightedAvg [p.vx, vx] [traction, 1-traction] else
      stepV vx (x < ball.r-halfWidth)) (x > halfWidth-ball.r)
      hitCeiling = (y > halfHeight - ball.r)
      ball' = stepObj t {ball | vx = newVx ,
                                vy = stepV vy hitPlayer hitCeiling }
    in
      (List.foldr goBrickHits (ball', []) bricks, contacts')

goBrickHits: Brick -> (Ball, List Brick) -> (Ball, List Brick)
goBrickHits brick (ball, bricks) =
  let
    hit = ball `within` brick
    bricks' = if hit then bricks else brick::bricks
    ball' = if hit then speedUp {ball | vy = -ball.vy} else ball
  in
    (ball', bricks')

gameState : Signal Game
gameState = foldp stepGame defaultGame input

-- view

main : Signal Element
main = Signal.map show gameState

-- yeah okay i'm kind of over this. it makes a lot of sense, i guess, if you know haskell...

-- but i don't know haskell, nor do i really give half a fuck about it, at least right now.
