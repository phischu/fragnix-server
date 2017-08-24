module Main where


import FragnixServer (
  application)
import Network.Wai.Handler.Warp (
  run)


main :: IO ()
main = run 8081 application

