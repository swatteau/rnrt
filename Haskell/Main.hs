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
import System.Exit

import Rnrt

readPositiveInteger :: String -> IO Integer
readPositiveInteger arg =
    let value = read arg :: Integer
    in if value > 0
           then return value
           else die ("Expected positive integer but got " ++ show value)

readPositiveRational :: String -> IO Rational
readPositiveRational arg =
    let value = read arg :: Rational
    in if value > 0
           then return value
           else die ("Expected positive rational but got " ++ show value)

main :: IO ()
main = do
  [arg_n, arg_q, arg_eps] <- getArgs
  n <- readPositiveInteger arg_n
  q <- readPositiveRational arg_q
  eps <- readPositiveRational arg_eps
  let root = rnrt n q eps
  let decimal = fromRational root
  putStrLn $ "Rational approximation: " ++ show root
  putStrLn $ "Decimal approximation: " ++ show decimal
  putStrLn $ "Control: " ++ (show . fromRational) (root ^ n)


