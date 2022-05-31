-- module ChessPiece (ChessPiece, toChessPiece) where
-- Define your ChessPiece type here.  Do not change the above module line.
import Data.Char (toUpper)
import Data.List
import Data.Maybe
import Text.Read (readMaybe)
data Colour = B | W deriving (Eq, Show)
data Type = K | Q | R | Bl | N | P deriving (Eq, Show)
data ChessPiece = ChessPiece Colour Type
    deriving (Eq, Show)

-- toChessPiece :: String -> Maybe ChessPiece
-- toChessPiece [x,y] = Just Colour x Type y
-- toChessPiece _ = Nothing


toCard :: String -> Maybe ChessPiece
-- toCard [] = Nothing
-- toCard (x:xs) = let mSuit = readMaybe [x] :: Colour
--                     mRank = readMaybe xs :: Type
--                 in case (mSuit, mRank) of
--                     (Just s, Just r) -> Just $ ChessPiece s r
--                     _ -> Nothing

 toCard [x,y] = do mSuit <- readMaybe x
                    mRank <- readMaybe y
                    return $ ChessPiece mSuit mRank


-- toChessPiece [X, Y] = 
--     let
--     maybeX = case toUpper X of { 'W' -> Just W; 'B' -> Just B; _ -> Nothing}
--     maybeY = case toUpper Y of { 'K' -> Just K; 'Q' -> Just Q; 'R' -> Just R;
--         'B' -> Just Bl; 'N' -> Just N; 'P' -> Just P; _ -> Nothing}
--     in case (maybeX, maybeY) of
--     (x, y) -> Just(ChessPiece (fromJust x) (fromJust y))
-- toChessPiece _ = Nothing
    