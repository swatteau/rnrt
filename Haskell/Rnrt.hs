-- Copyright 2016 Sébastien Watteau
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
module Rnrt (closest, rnrt) where

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Ratio

mediant :: Rational -> Rational -> Rational
mediant a b = (numerator a + numerator b) % (denominator a + denominator b)

inverse :: Rational -> Rational
inverse r = (denominator r) % (numerator r)

type SBNode = (Rational, Rational)

value :: SBNode -> Rational
value (a, b) = mediant a b

left :: SBNode -> SBNode
left node@(a, b) = (a, value node)

right :: SBNode -> SBNode
right node@(a, b) = (value node, b)

closest :: Integer -> Rational -> Rational -> Rational
closest n q eps =
    if q == 1%1
        then q
        else let seq = rnrt n q
             in head $ dropWhile (\r -> abs (q - r ^ n) > eps ) seq

rnrt :: Integer -> Rational -> [Rational]
rnrt n q =
    let initError = q
    in if q > 1
         then map inverse $ evalState (rnrt' n (inverse q)) initError
         else evalState (rnrt' n q) initError

rnrt' :: Integer -> Rational -> State Rational [Rational]
rnrt' n q = go (0, 1)
  where
    go :: SBNode -> State Rational [Rational]
    go node = do
        smallestError <- get
        let nodeValue = value node
            candidate = nodeValue ^ n
            currError = q - candidate
        if abs currError < abs smallestError
            then do put currError
                    (:) nodeValue <$> selectNext node currError
            else selectNext node currError

    selectNext :: SBNode -> Rational -> State Rational [Rational]
    selectNext node error
        | error < 0 = go (left node)
        | error > 0 = go (right node)
        | otherwise = return []

