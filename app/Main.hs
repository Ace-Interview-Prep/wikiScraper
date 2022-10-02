{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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
import Data.List


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




type Genre = Text 
type OriginalGenre = Genre

data ResultOut = ResultOut { genre :: (OriginalGenre, Depth)
                           , webId :: WebPointer
                           , paragraph :: Text
                           } deriving (Generic)

instance Aeson.ToJSON ResultOut
instance Aeson.FromJSON ResultOut

type WebPointer = (Link, ElemHead)
type ID_From = (ElemHead, ElemHead) -- Shell and Target
type ParagraphID = String
type LinkElem = Elem' String
type ThisLink = Link
type Paragraph = Text
type Depth = Int
type MaxDepth = Int


  


-------------------------------------------------------------------------------------------------------------------

type TheFuckingWeb = BiDMap 
-- which could be further expanded using
data Link' = SameSite Link
           | ForeignRef Link 

data RefsOut = Links [Link]
             | EmptyLeaf -- Literal dead end
             | RichLeaf deriving (Show, Eq, Generic)

instance Aeson.ToJSON RefsOut
instance Aeson.FromJSON RefsOut
             
             -- We purposely chose to stop here. The scraper should not scrape until infinity before going
             -- to another term. arguably, this should take very small steps eg. do 5 depth for
             -- each term / RichLeaf then cycle to the next 

data RefIn = Start Genre -- The link is the Key itself 
           | Transient Genre Depth Link 
           deriving (Show, Eq, Generic)

instance Aeson.ToJSON RefIn
instance Aeson.FromJSON RefIn

instance Aeson.ToJSONKey Link
instance Aeson.FromJSONKey Link

-- | It is worth noting that with Maps, if we want the concept of a WebPointer, we can achieve this exactly
-- | with parseOpeningTagF (== href) and can recover the containers by asking the link elem to be contained
-- | as a match

  -- A page is a singleton that gets merged into a map which it led/linked to
  -- this means we can control hierarchy : if a parent finds that a descendant references it, we can
  -- specially label this 


  -- If this is parameterized with RefIn then I can return the Map
  -- This means that a parent and child should have the same key

  -- 1) Parent:                fromList [(Link "A", payload@(Links [Link "B"], [..], [..])) , (Link "B", RichLeaf, [Transient "GENRE" 23 (Link "A"), []]
  -- 2) "Child" (not really):  fromList [(Link "B", payload@(Links [Link "C"], [..], [..])) , <<<<<<<<< node c,d,e,f >>>>>>>>>>>>>>>>>>>>>

  -- SO both know of "B" but have different ideas of them, so we must know which is parent and this is evident in the merge
  -- so we deduce this and then defer to the child for Paragraphs and RefsOut

  -- SO each page should NOT create a singleton Map but instead a Map with its own ref in it

-- TODO(galen): could this generalize past wikis
data Page = Page { idLink :: Link
                 , genreInstance :: (OriginalGenre, Depth)
                 , paragraphs :: [Text]
                 , externalReferences :: [Link]
                 -- This implies we use any wiki link to build tree
                 -- And leave external refs for later 
                 , categories :: [Text]
                 } deriving (Generic)-- Aeson.ToJSON, Aeson.FromJSON)

instance Aeson.ToJSON Page
instance Aeson.FromJSON Page


scrapeWikiPage :: (OriginalGenre, Depth) -> Link -> Html -> ([Link], Page)
scrapeWikiPage genInst link html =
  let 
  
    res :: [(Elem' String, [LinkElem])]
    res = fromMaybe [] $ scrape pTagWithLinks html

    f :: Link -> Bool
    f (Link l) = not $ elem '#' l

    links :: [Link]
    links = filter f $ catMaybes $ fmap (getHrefEl True link) $ mconcat $ fmap snd res
    
    paras = fmap (pack . innerHtmlFull)

--    referenceElems = flip scrape html $ el "div" [("class", "reflist")] `contains'` el "a" []
    referenceElems = flip scrape html $ elemParser (Just ["a"]) noPat [("class", Just "external text")]
---  fmap (getHrefEl False link) $ fromMaybe [] $

    references = catMaybes $ fmap (getHrefEl False link) $ fromMaybe [] $ referenceElems

    
    categories = flip scrape html $ contains' (el "div" [("id", "mw-normal-catlinks")]) $ do 
      e <- elemParserWhere (Just ["a"]) noPat "href" (isPrefixOf "/wiki/Category:")
      let
        innards = innerHtmlFull e
      pure $ (strip . pack) innards

    categories' = fromMaybe [] $ mconcat <$> categories
        
  in 
    (links, Page link genInst (paras $ fmap fst res) references categories')


-- test = do
--   mgr <- newManager tlsManagerSettings
--   (_, html) <- getHtml mgr testCase
--   testscrapeWikiPage ("Networking", 1) testCase html 

-- testCase = Link "https://en.wikipedia.org/wiki/Packet-switching"


-- <a rel="nofollow" class="external text" href="https://web.archive.org/web/20160324033133/http://www.packet.cc/files/ev-packet-sw.html">"The Evolution of Packet Switching"</a>




-- testscrapeWikiPage :: (OriginalGenre, Depth) -> Link -> Html -> IO ([Link], Page)
-- testscrapeWikiPage genInst link html =
--   let 
  
--     res :: [(Elem' String, [LinkElem])]
--     res = fromMaybe [] $ scrape pTagWithLinks html

--     links :: [Link]
--     links = catMaybes $ fmap (getHrefEl True link) $ mconcat $ fmap snd res
    
--     paras = fmap (pack . innerHtmlFull)

-- --    referenceElems = flip scrape html $ el "div" [("class", "reflist")] `contains'` el "a" []

--     referenceElems = flip scrape html $ elemParser (Just ["a"]) noPat [("class", Just "external text")]
    
-- ---  fmap (getHrefEl False link) $ fromMaybe [] $

--     --references = catMaybes $ fmap (getHrefEl False link) $ mconcat $ fromMaybe [] $ referenceElems

    
--     categories = flip scrape html $ contains' (el "div" [("id", "mw-normal-catlinks")]) $ do 
--       e <- elemParserWhere (Just ["a"]) noPat "href" (isPrefixOf "/wiki/Category:")
--       let
--         innards = innerHtmlFull e
--       pure $ (strip . pack) innards

--     categories' = fromMaybe [] $ mconcat <$> categories
        
--   in do
--     print $ length res
--     print "-------------------------------------------------------------------------------------------"
--     print $ length links
--     print "-------------------------------------------------------------------------------------------"    
--     print $ length $  paras $ fmap fst res
--     print "-------------------------------------------------------------------------------------------"    
--     print $ length $ fromJust referenceElems
--     print "-------------------------------------------------------------------------------------------"    
-- --    print $ length references
--     print "-------------------------------------------------------------------------------------------"    
--     print $ length categories'
--     pure $ (links, Page link genInst (paras $ fmap fst res) undefined categories')


-- If i change to a Page datastructure, this means there is singular file pertaining to each page, less
-- duplication and easier time relating paragraphs to each other (like for the application of contiguity)



-- Parent <---------------------> Child <--------------------...--> 

-- "I birthed this child"         "I am child with these things"
--                                also "I birthed this child" 


-- However instead to be more efficient I can say


-- "Hey child I birthed you, my name (reference) is X"

-- so that each Offspring can say:

--   - I am child with this ID
--   - I was birthed by Reference
--   - My children that I plan to birth are: [a .. b] 

--   and do:
--    - birthChildWith "Hey child I birthed you, my name (reference) is X2" 


type BiDMap = Map.Map Link Payload'

type Parent = RefIn

-- Handle same page, all should be the same except refsIn
-- This should only ever run when the Link matches one in the Map 
insertFunction :: Payload' -> Payload' -> Payload'
insertFunction new@(refsOut_N, (refsIn_N:[]), pageId_N) old@(refsOut, refsIn, pageId_O) =
  -- there should only be one in refsIn of new
  if refsOut == refsOut_N && pageId_O == pageId_N -- law for a page 
  then (refsOut, refsIn_N:refsIn, pageId_O) 
  else error "this should be impossible" 


getGenre :: RefIn -> Genre
getGenre = \case
  Transient genre _ _ -> genre
  Start g -> g

-- | TODO(galen): Have we handled how a leaf should look? such that
-- |              we know it hasnt been processed and can be continued on?
-- | TODO(galen): This doesnt show if the links found were none
--type Payload' = (RefsOut, [RefIn], PageID)
runTreeMapStateT :: Manager -> RefIn -> (Depth, MaxDepth) -> Link -> StateT BiDMap IO ()
runTreeMapStateT mgr refIn (depthPrev, maxDepth) link = do
  liftIO $ print link
  
  (_, html) <- liftIO $ getHtml mgr link
  let
    depth = depthPrev + 1 
    genre = getGenre refIn
    (links, wikiPage) = scrapeWikiPage (genre, depth) link html

  startEvalPage <- liftIO getCurrentTime 
  liftIO $ writePage genre depth wikiPage
  endEvalPage <- liftIO getCurrentTime
  liftIO $ print (diffUTCTime endEvalPage startEvalPage)
  let
    pageId = pack . show $ hmacGetDigest $ hmacAlg SHA256 "myKey" $ toStrict $ Aeson.encode wikiPage
    thisPayload = (Links links, [refIn], pageId)

  -- This insert or edit does not know about other Parents --------------------------------
  start <- liftIO $ getCurrentTime 
  modify (\stateMap -> StrictMap.insertWith insertFunction link thisPayload stateMap)
  stop <- liftIO $ getCurrentTime
  
  liftIO $ print (diffUTCTime stop start)
  ----------------------------------------------------------------------------------------

  -- This will update state from all of the Children of this page 
  case depth == maxDepth of
    True -> pure ()
    False -> do
      -- filter links for ones that are already keys in State
      keysState <- gets Map.keys 
      let links' = filter (\x -> not $ elem x keysState) links 
      mapM_ (runTreeMapStateT mgr (Transient genre (depth) link) (depth, maxDepth)) links
  pure () 


type Payload' = (RefsOut, [RefIn], PageID)
type PageID = Text

writePage :: Genre -> Depth -> Page -> IO ()
writePage genre depth page = do
  let pageId = show $ hmacGetDigest $ hmacAlg SHA256 "" $ toStrict $ Aeson.encode page   
  let dir = "/home/lazylambda/code/Ace/wikiResults/" <> (unpack genre) <> "/" <> (show depth) <> "/"
  let path = dir <> pageId <> ".json"
  createDirectoryIfMissing True dir
  Aeson.encodeFile path page


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

  -- --print undefined -- placeholder to remind me to
  appendFile "completed.log" $ unpack genre <> ","
  Aeson.encodeFile (unpack genre <> ".json") mappy 
  
--  print mappy 
  pure ()

pTagWithLinks :: ScraperT (Elem' String, [Elem' String])
pTagWithLinks = do
  e <- el "p" []
  let txt = innerHtmlFull e
  eiths <- case parse f "in p tag" txt of
    Right x -> pure x 
    --Right (words, links) -> pure (e { innerHtmlFull = words }, links)
    Left _ -> error "idk ... weird" 


  let words = g eiths
  -- True because we'd like to stay on the same site
  -- TODO(galen): split into ([SameSite], [OtherSite]) and append to a file with some way to do this later
  -- and add to the tree 
  let links = {-fmap (getHrefEl True cLink)-} (catEithers eiths)

  pure (e { innerHtmlFull = words } , links)             
      

  where
    g (x:xs) = case x of
      Right e -> innerHtmlFull e <> (g xs) 
      Left chr -> chr : (g xs)
    g [] = []
    
    f = many ((Right <$> (try $ el "a" [])) <|> (Left <$> anyChar))

  




