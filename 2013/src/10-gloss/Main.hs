module Main where

import Control.Monad (void)
import Control.Lens(ix, (.~))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, newEmptyMVar, tryTakeMVar)
import Graphics.Gloss.Interface.IO.Game (playIO, Display(InWindow), azure, Picture,
    line, black, color, translate, white, thickCircle, Event(EventKey),
    MouseButton(LeftButton), Key(MouseButton), KeyState(Up))
import Data.Monoid ((<>))
import System.Random (randomRIO)

data Play = X | O deriving Eq

type Board = [[Maybe Play]]

initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Nothing)

main :: IO ()
main = do
    aiMove <- newEmptyMVar
    playIO
        (InWindow "TicTacToe" (1, 1) (500, 500))
        azure
        10
        (initialBoard, X)
        drawBoard
        (handleInput aiMove)
        (stepGame aiMove)

drawBoard :: (Board, Play) -> IO Picture
drawBoard (board, _) = return (grid <> plays)
    where
        grid :: Picture
        grid =
            color black (line [ (-100, -300), (-100, 300) ]) <>
            color black (line [ (100, -300), (100, 300) ]) <>
            color black (line [ (-300, 100), (300, 100) ]) <>
            color black (line [ (-300, -100), (300, -100) ])
        plays = mconcat
            [ translate (fromIntegral ((x - 1) * 200))
                        (fromIntegral ((y - 1) * 200)) $
                case play of
                    X -> color white (thickCircle 1 50)
                    O -> color black (thickCircle 1 50)
            | x <- [0..2]
            , y <- [0..2]
            , Just play <- [ (board !! x) !! y ]
            ]

handleInput :: MVar Board -> Event -> (Board, Play) -> IO (Board, Play)
handleInput
    aiMove
    (EventKey (MouseButton LeftButton) Up _ (x, y))
    (board, X) =
        let snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 100) . (+ 50)
            (gridX, gridY) = (snap x, snap y)
        in case (board !! gridX) !! gridY of
            Just _ -> return (board, X)
            Nothing -> do
                let newBoard = ix gridX . ix gridY .~ (Just X) $ board
                forkAi aiMove newBoard
                return (newBoard, O)
handleInput _ _ x = return x

stepGame :: MVar Board -> Float -> (Board, Play) -> IO (Board, Play)
stepGame aiMove _ (board, O) =
    tryTakeMVar aiMove >>=
        return .
            maybe (board, O)
                  (\newBoard -> (newBoard, X))
stepGame _ _ state = return state

forkAi :: MVar Board -> Board -> IO ()
forkAi aiMove board = void $ forkIO $ do
    randomRIO (100000, 1000000) >>= threadDelay

    let plays = [ (ix x . ix y .~ Just O) board
                | x <- [0..2]
                , y <- [0..2]
                , Nothing <- [ (board !! x) !! y ]
                ]

    case plays of
        [] -> do
            putMVar aiMove board

        _ -> do
            newBoard <- (plays !!) <$> randomRIO (0, length plays - 1)
            putMVar aiMove newBoard
