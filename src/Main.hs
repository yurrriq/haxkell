{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" $ text "Hello, world!"
  get "/hi" $ do
    agent <- header "User-Agent"
    text $ mconcat ["Hello, ", fromMaybe "world" agent, "!"]
