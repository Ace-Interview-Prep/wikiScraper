{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-

I need to expand this so that I can keep a genre only folder so that I can audit the webscraping behaviour

However the core folder will be simply a flat folder
Although for filesystem constraints we may need a folder.json which points to which folder the file is actually in
although it may be cheaper to just search each folder for the file 

-}

module Main where

import qualified Sequential
import qualified Concurrent

main :: IO ()
main = Concurrent.main
  -- mgr <- newManager tlsManagerSettings
  -- --(_, html) <- getHtml mgr $ Link "https://en.wikipedia.org/wiki/Network_topology"

  -- let readFile' x = readFile x >>= pure . mconcat . fromRight [] . parseCSV "" 
  -- genres <- (,,) <$> (readFile' "genresUWO.txt") <*> (readFile' "genresWLU.txt") <*> (readFile' "genresUO.txt") >>= \(a,b,c) -> pure $ fmap pack $  a <> b <> c



      
