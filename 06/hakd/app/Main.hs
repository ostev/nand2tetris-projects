module Main (main) where

import Hack.Assemble

main :: IO ()
main = print $ assemble "D=2"
