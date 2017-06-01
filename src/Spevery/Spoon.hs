{-# LANGUAGE OverloadedStrings #-}
module Spevery.Spoon (getSpoons) where

import           Control.Monad (unless)
import           Control.Monad.State
import           Data.Char (isLetter, isSpace)
import           Data.List (permutations)
import           Data.Maybe (catMaybes)
import           Data.Set (fromList, member)
import qualified Data.Text as T


isV :: Char -> Bool
isV c = c `member` fromList ['a', 'e', 'i', 'o', 'u', 'y']

split3 :: T.Text -> Maybe (Char, Char, T.Text)
split3 t = do (h0, t0) <- T.uncons t
              (h1, t1) <- T.uncons t0
              return (h0, h1, t1)

splitOnConsCluster' :: State (T.Text, T.Text) ()
splitOnConsCluster' = do (h, t) <- get
                         if T.null h && T.length t == 1
                            then put (t, "")
                            else case split3 t of
                                   Nothing -> return ()
                                   Just ('q', 'u', t') -> put ("qu", t')
                                   Just (h0, h1, t') -> unless (isV h0) $ do
                                     put (T.snoc h h0, T.cons h1 t')
                                     splitOnConsCluster'

splitOnConsCluster :: T.Text -> Maybe (T.Text, T.Text)
splitOnConsCluster s | T.null s = Nothing
                     | otherwise = Just $ execState splitOnConsCluster' ("", s)


ac :: Eq a => [a] -> [a] -> Bool
ac a1 a2 = and $ zipWith (/=) a1 a2

combine :: [(T.Text, T.Text)] -> [[T.Text]]
combine a = let (hs, ts) = unzip a
             in [zipWith T.append p ts | p <- permutations hs, p `ac` hs]

notPunc :: Char -> Bool
notPunc = (||) <$> isLetter <*> isSpace

getSpoons :: T.Text -> [[T.Text]]
getSpoons = combine . catMaybes . fmap splitOnConsCluster
          . T.words . T.toLower . T.filter notPunc

