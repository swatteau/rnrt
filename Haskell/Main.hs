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
module Main where

import Data.Ratio
import System.Environment

import Rnrt

closest :: Integer -> Rational -> Rational -> Rational
closest n q eps =
    let seq = rnrt n q
    in head $ dropWhile (\r -> abs (q - r ^ n) > eps ) seq


main :: IO ()
main = do
  [arg_n, arg_q, arg_eps] <- getArgs
  let n = read arg_n :: Integer
  let q = read arg_q :: Rational
  let eps = read arg_eps :: Rational
  let result = closest n q eps
  let decimal = fromRational result
  putStr "Closest rational number: "
  print result
  putStr "Decimal approximation: "
  print decimal
  putStr "Control: "
  print (decimal ^ n)


