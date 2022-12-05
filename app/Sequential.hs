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


module Sequential where


----------------------
import ScrapeWiki
import Types






-------------------------



import StartGenre (startGenre)

import Scrappy.Requests
import Scrappy.Scrape
import Scrappy.Elem.ChainHTML
import Scrappy.Elem.SimpleElemParser
import Scrappy.Elem.Types (getHrefEl, innerHtmlFull, Elem', ElemHead, attrs, elTag, noPat)
import Scrappy.Links
import Scrappy.BuildActions (catEithers) -- TODO: Move to Scrappy.Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, modify, runStateT, gets)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Parsec (anyChar, (<|>), parse, many, try)
import Control.Applicative (liftA2)
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import Data.Text (Text, pack, unpack, strip)
import Data.Text.Encoding (encodeUtf8)



import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap
import Data.Tree
import Data.Either (fromRight)
import qualified Data.Aeson as Aeson
import GHC.Generics
import System.Directory
import Crypto.Hash (SHA256(SHA256))
import Crypto.MAC (hmacAlg, hmacGetDigest)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (toStrict)
import Text.CSV



--type Payload' = (RefsOut, [RefIn], PageID)
-- Handle same page, all should be the same except refsIn
-- This should only ever run when the Link matches one in the Map 
insertFunction :: Payload' -> Payload' -> Payload'
insertFunction new@(refsOut_N, (refsIn_N:[]), pageId_N) old@(refsOut, refsIn, pageId_O) =
  (refsOut, refsIn_N:refsIn, pageId_O) 
  -- there should only be one in refsIn of new
  -- if refsOut == refsOut_N --  && pageId_O == pageId_N -- law for a page 
  -- then (refsOut, refsIn_N:refsIn, pageId_O) 
  -- else error "this should be impossible" 










-- | TODO(galen): Have we handled how a leaf should look? such that
-- |              we know it hasnt been processed and can be continued on?
-- | TODO(galen): This doesnt show if the links found were none
--type Payload' = (RefsOut, [RefIn], PageID)
runTreeMapStateT :: Manager -> RefIn -> (Depth, MaxDepth) -> Link -> StateT (Map.Map Link [RefIn]) IO ()
runTreeMapStateT mgr refIn (depthPrev, maxDepth) link = do
  liftIO $ print link
  liftIO $ print (depthPrev + 1) 
  (_, html) <- liftIO $ getHtml mgr link
  let
    depth = depthPrev + 1 
    genre = getGenre refIn
    (links, wikiPage) = scrapeWikiPage (genre, depth) link html

  startEvalPage <- liftIO getCurrentTime 
  liftIO $ writePage genre depth link wikiPage 
  endEvalPage <- liftIO getCurrentTime
  liftIO $ print (diffUTCTime endEvalPage startEvalPage)
  let
    --toStrict $ Aeson.encode wikiPage
    pageId = toHashPath link
    thisPayload = (Links links, [refIn], pageId)

  -- This insert or edit does not know about other Parents --------------------------------
  start <- liftIO $ getCurrentTime 
  modify (\stateMap -> Map.insertWith (<>) link [refIn] stateMap )
  stop <- liftIO $ getCurrentTime
  
  liftIO $ print (diffUTCTime stop start)
  ----------------------------------------------------------------------------------------

  -- This will update state from all of the Children of this page 
  case depth == maxDepth of
    True -> pure ()
    False -> do
      -- filter links for ones that are already keys in State
      mapM_ (runTreeMapStateT mgr (Transient genre (depth) link) (depth, maxDepth)) links
  pure () 


-- | We should also get Categories listed and external references 
-- | We could also track what genres Wikipedia has
  -- Categories on wikipedia could even be well-defined sub genres 


advSearchUrl = "https://en.wikipedia.org/w/index.php?search=&title=Special:Search&profile=advanced&fulltext=1&ns0=1"


main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  --(_, html) <- getHtml mgr $ Link "https://en.wikipedia.org/wiki/Network_topology"

  let readFile' x = readFile x >>= pure . mconcat . fromRight [] . parseCSV "" 
  genres <- (,,) <$> (readFile' "genresUWO.txt") <*> (readFile' "genresWLU.txt") <*> (readFile' "genresUO.txt") >>= \(a,b,c) -> pure $ fmap pack $  a <> b <> c


  mapM_ (runGenre mgr) genres 

runGenre :: Manager -> Genre -> IO ()
runGenre mgr genre = do
  putStrLn "----------------------------------------------------------------------------------------------------"
  putStrLn "----------------------------------------------------------------------------------------------------"
  putStrLn "----------------------------------------------------------------------------------------------------"
  putStrLn "----------------------------------------------------------------------------------------------------"
  print $ "NEXT genre: " <> genre
  putStrLn "----------------------------------------------------------------------------------------------------"
  putStrLn "----------------------------------------------------------------------------------------------------"
  putStrLn "----------------------------------------------------------------------------------------------------"
  putStrLn "----------------------------------------------------------------------------------------------------"

  start <- startGenre mgr $ unpack genre 
  appendFile "genre<->start.log" $ show (genre, start) <> "," 
  (_, mappy) <- runStateT (runTreeMapStateT mgr (Start genre) (0, 10) start) mempty

  mergeMappyWithFiles mappy -- placeholder 

  -- --print undefined -- placeholder to remind me to
  appendFile "completed.log" $ unpack genre <> ","
  Aeson.encodeFile (unpack genre <> ".json") mappy 
  
--  print mappy 
  pure ()

mergeMappyWithFiles :: Map.Map Link [RefIn] -> IO ()
mergeMappyWithFiles = mapM_ mergeMappyCaseWithFile . Map.toList 

-- | The page id is both the hashing of the given Link
-- | and the filepath of the page. We only know the refs in at the end so we must re-merge
-- | All we intend to do here is update the page with 
-- | LAW: hash link == filepath  
mergeMappyCaseWithFile :: (Link, [RefIn]) -> IO ()
mergeMappyCaseWithFile (link, refsIn) = do

  let filePath = toHashPath link


  page :: Maybe Page <- Aeson.decodeFileStrict filePath

  -- TODO(galen): Instead return a list of errors that accumulate
  
  Aeson.encodeFile filePath $ refdInPage refsIn $ fromJust page

refdInPage :: [RefIn] -> Page -> RefdPage
refdInPage refsIn (Page idLink genInst paras sameSiteLinks externalReferences categories) =
  RefdPage idLink genInst paras sameSiteLinks refsIn externalReferences categories



-- | Somehow was scraped 
-- | https://en.wikipedia.org/https://www.perseus.tufts.edu/hopper/text?doc=Perseus:text:1999.04.0057:entry=platu/s             












------------------------------CONCURRENCY

-- data STM_RefIn = STM_RefIn RefIn [Link] 

-- data Level = Level Genre Depth [STM_RefIn]





-- -- What about when the Level is massive

-- data Level' = Level' Genre Depth TableIdentifier

-- -- We also have to output to a table





-- data Table = Table Depth Genre [STM_RefIn]

-- -- | Except Really:

-- data LevelTable = LevelTable
--   { _genre :: Columnar f Genre
--   ,



    

    







-- [RefIn]



--   RefInFile



--   [ From , [To, To, To, To, To]
--   , From , [To, To, To, To, To]
--   ..
--   ]

--   .chunk file




-- a .chunk file should be the size of the computers RAM divided up by the number of concurrent processes

