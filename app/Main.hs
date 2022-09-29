{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Scrappy.Requests
import Scrappy.Scrape
import Scrappy.Elem.ChainHTML
import Scrappy.Elem.SimpleElemParser
import Scrappy.Elem.Types (getHrefEl, innerHtmlFull, Elem', ElemHead, attrs, elTag, noPat)
import Scrappy.Links
import Scrappy.BuildActions (catEithers) -- TODO: Move to Scrappy.Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, modify)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Parsec (anyChar, (<|>), parse, many, try)
import Control.Applicative (liftA2)
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import Data.Text (Text, pack, unpack, strip)
import Data.Text.Encoding (encodeUtf8)
import Data.List

import qualified Data.Map as Map
import Data.Tree 
import qualified Data.Aeson as Aeson
import GHC.Generics
import System.Directory
import Crypto.Hash (SHA256(SHA256))
import Crypto.MAC (hmacAlg, hmacGetDigest)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (toStrict)

-- data RetraceableTree = RetraceableTree (Map Link ([Paragraph], RetraceableTree))
--   deriving (Semigroup, Monoid, Show)



--type RetraceableTree = Map Link ([Paragraph], RetraceableTree)

--PSEUDO
growLeaf :: Label -> IO (Tree Label)
growLeaf = undefined
--growLeaf (_, url, _) = runTree depthAllowed (depth->0) url 


--type RetraceableTree' = Tree (Link [ElemHead])

-- uncurry(1)
-- >> Map Link ([Paragraph], Map Link 

-- concept:


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


  

-- | We add in OriginalGenre so that we can merge and preserve 
type Label = (ID_From, OriginalGenre, ThisLink, [ParagraphID]) 
runTree :: Manager -> OriginalGenre -> (Depth, MaxDepth) -> (ID_From, ThisLink) -> IO (Tree Label)
runTree mgr genre (depth, maxDepth) (id_from, link) = do
  (_, html) <- getHtml mgr link
  let
    res :: [(Elem' String, [LinkElem])]
    res = fromMaybe [] $ scrape pTagWithLinks html

    labelLinks :: (Elem' String, [LinkElem]) -> [(ID_From, Link)] 
    labelLinks (shell, elems) =
      let
        eHeadShell = (elTag shell, attrs shell)
        filterMaybes i = (fmap.fmap) fromJust $ (filter (\(_, x) -> isJust x)) i
        targetLinksWithLabel = filterMaybes $ fmap (\e -> ((elTag e, attrs e), getHrefEl True link e)) elems
      in
        fmap (bimap (eHeadShell,) id) $ targetLinksWithLabel
    
    labeledLinks :: [(ID_From, Link)]
    labeledLinks = mconcat $ fmap labelLinks res

    toResult e = ResultOut (genre, depth) (link, (elTag e, attrs e)) (pack $ innerHtmlFull e)

    -- Importante! This is signing the ResultOut with the genre and depth
    -- So that there's never a collision
    -- We could still do flat analysis of the result dir to compare stuff
    toSignedResult :: ResultOut -> (ParagraphID, ResultOut)
    toSignedResult r =
      let
        x = encodeUtf8 $ genre <> (pack . show $ depth)
        y = toStrict . Aeson.encode $ r
      in 
        (show $ hmacGetDigest $ hmacAlg SHA256 x y, r)

    signedReferencedParagraphs :: [(ParagraphID, ResultOut)] 
    signedReferencedParagraphs = fmap (toSignedResult . toResult . fst) res  

  let paraIds = fmap fst signedReferencedParagraphs

  mapM (writeResult genre depth) signedReferencedParagraphs
  -- I could change this so that its a Map
  -- then just Merge the maps and this will typecheck 
  forest <- case depth == maxDepth of
    True -> pure []
    False -> mapM (runTree mgr genre (depth+1, maxDepth)) labeledLinks

  let label = (id_from, genre, link, paraIds)

  pure $ Node label forest 


-------------------------------------------------------------------------------------------------------------------

-- | ALTERNATE APPROACH
-- SIMPLE BIDIRECTIONAL MAP 
type SiteBidirectionalMap = Map.Map Link Payload
--type BidirectionalMap a (b :: * -> *) = Map a (b a) 
-- Note that this implies the whole internet could be mapped as

type TheFuckingWeb = Map.Map BaseUrl SiteBidirectionalMap
-- which could be further expanded using
data Link' = SameSite Link
           | ForeignRef Link 

data RefsOut = Links [Link]
             | EmptyLeaf -- Literal dead end
             | RichLeaf
             deriving (Show, Eq)
             -- We purposely chose to stop here. The scraper should not scrape until infinity before going
             -- to another term. arguably, this should take very small steps eg. do 5 depth for
             -- each term / RichLeaf then cycle to the next 

data RefIn = Start Genre -- The link is the Key itself 
           | Transient Genre Depth Link 
           deriving (Show, Eq)
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

    links :: [Link]
    links = catMaybes $ fmap (getHrefEl True link) $ mconcat $ fmap snd res
    
    paras = fmap (pack . innerHtmlFull)

    referenceElems = flip scrape html $ el "div" [("class", "reflist")] `contains'` el "a" []
---  fmap (getHrefEl False link) $ fromMaybe [] $

    references = catMaybes $ fmap (getHrefEl False link) $ mconcat $ fromMaybe [] $ referenceElems

    
    categories = flip scrape html $ contains' (el "div" [("id", "mw-normal-catlinks")]) $ do 
      e <- elemParserWhere (Just ["a"]) noPat "href" (isPrefixOf "/wiki/Category:")
      let
        innards = innerHtmlFull e
      pure $ (strip . pack) innards

    categories' = fromMaybe [] $ mconcat <$> categories
        
  in 
    (links, Page link genInst (paras $ fmap fst res) references categories')

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
insertFunction old@(refsOut, refsIn, pageId_O) new@(refsOut_N, (refsIn_N:[]), pageId_N) =
  -- there should only be one in refsIn of new
  if refsOut == refsOut_N && pageId_O == pageId_N -- law for a page 
  then (refsOut, refsIn_N:refsIn, pageId_O) 
  else error "this should be impossible" 


getGenre :: RefIn -> Genre
getGenre = \case
  Transient genre _ _ -> genre
  Start g -> g


--type Payload' = (RefsOut, [RefIn], PageID)
runTreeMapStateT :: Manager -> RefIn -> (Depth, MaxDepth) -> Link -> StateT BiDMap IO ()
runTreeMapStateT mgr refIn (depthPrev, maxDepth) link = do
  liftIO $ print link
  
  (_, html) <- liftIO $ getHtml mgr link
  let
    depth = depthPrev + 1 
    genre = getGenre refIn
    (links, wikiPage) = scrapeWikiPage (genre, depth) link html

  liftIO $ writePage genre depth wikiPage
  let
    pageId = pack . show $ hmacGetDigest $ hmacAlg SHA256 "myKey" $ toStrict $ Aeson.encode wikiPage
    thisPayload = (Links links, [refIn], pageId)

  -- This insert or edit does not know about other Parents
  modify (\stateMap -> Map.insertWith insertFunction link thisPayload stateMap)

  -- This will update state from all of the Children of this page 
  case depth == maxDepth of
    True -> pure ()
    False -> do
      mapM_ (runTreeMapStateT mgr (Transient genre (depth) link) (depth, maxDepth)) links
  pure () 


type Payload' = (RefsOut, [RefIn], PageID)
runTreeMap' :: Manager -> OriginalGenre -> (Depth, MaxDepth) -> Link -> IO (Map.Map Link Payload')
runTreeMap' mgr genre (depth, maxDepth) link = do
  print link 
  (_, html) <- getHtml mgr link
  let (links, wikiPage) = scrapeWikiPage (genre, depth) link html
  writePage genre depth wikiPage

  descendantMap <- case depth == maxDepth of
    True -> pure mempty
    False -> do
      putStrLn "starting fork"
      maps <- mapM (runTreeMap' mgr genre (depth+1, maxDepth)) links
      print maps
      putStrLn "starting foldr op"
      pure $ foldr (Map.unionWithKey siblingUnion') mempty maps

  print descendantMap 
  putStrLn "done foldr op"
  let 
    toLinksMap :: [Link] -> Map.Map Link Payload'
    toLinksMap links = Map.fromList $ fmap (,(RichLeaf, [Transient genre depth link], mempty)) links   -- branches

  let f x@(out, inn, paras) y@(_, inn', _) = (out, inn <> (filter (\x -> not $ elem x inn) inn'), paras)
  let descendantMap' = Map.unionWith parentUnion' (toLinksMap links) descendantMap
  let pageId = show $ hmacGetDigest $ hmacAlg SHA256 "" $ toStrict $ Aeson.encode wikiPage   
  let thisMap = Map.insertWith f link (Links links, [], pack pageId) descendantMap'
  pure thisMap



-- [Payload..]

-- foldr f mempty payloads 

-- f :: Payload' -> Map Link Payload

-- If i have a database, each page, writes an entry to the database of some Payload'

-- This then means that I need another program which will recursively fetch from the database and build the resulting Map 

    
-- | We add in OriginalGenre so that we can merge and preserve 
type Payload = (RefsOut, [RefIn], [ParagraphID])
runTreeMap :: Manager -> OriginalGenre -> (Depth, MaxDepth) -> Link -> IO (Map.Map Link Payload)
runTreeMap mgr genre (depth, maxDepth) link = do
  (_, html) <- getHtml mgr link
  let wikiPage = scrapeWikiPage (genre, depth) link html 
  
  let
    res :: [(Elem' String, [LinkElem])]
    res = fromMaybe [] $ scrape pTagWithLinks html
    toResult e = ResultOut (genre, depth) (link, (elTag e, attrs e)) (pack $ innerHtmlFull e)

    toSignedResult :: ResultOut -> (ParagraphID, ResultOut)
    toSignedResult r =
      let
        x = encodeUtf8 $ genre <> (pack . show $ depth)
        y = toStrict . Aeson.encode $ r
      in 
        (show $ hmacGetDigest $ hmacAlg SHA256 x y, r)

    signedReferencedParagraphs :: [(ParagraphID, ResultOut)] 
    signedReferencedParagraphs = fmap (toSignedResult . toResult . fst) res  

    -- TODO: check if null 
    links :: [Link]
    links = catMaybes $ fmap (getHrefEl True link) $ mconcat $ fmap snd res

    -- The parent tells the outcome that the child has a RefIn, which is itself. 
    toLinksMap :: [Link] -> Map.Map Link Payload
    toLinksMap links = Map.fromList $ fmap (,(RichLeaf, [Transient genre depth link], [])) links   -- branches
      
  let paraIds = fmap fst signedReferencedParagraphs

  mapM (writeResult genre depth) signedReferencedParagraphs

  descendantMap <- case depth == maxDepth of
    True -> pure mempty
    False -> do
      maps <- mapM (runTreeMap mgr genre (depth+1, maxDepth)) links
      putStrLn "starting foldr op"
      pure $ foldr (Map.unionWithKey siblingUnion) mempty maps
      
  putStrLn "done folder op" 
  -- Should I combine "this page" before or after?
  -- the behaviour would be different for that one specifically 
  let descendantMap' = Map.unionWith parentUnion (toLinksMap links) descendantMap 

  -- change this to updateWith or something 
 -- let thisPageAsMap = Map.fromList [(link, (Links links, [passedRef], fmap fst signedReferencedParagraphs))] -- this page

  let f x@(out, inn, paras) y@(_, inn', _) = (out, inn <> (filter (\x -> not $ elem x inn) inn'), paras)
  
  {- having nothing here works as long as the first step is labeled properly -}
  let thisMap = Map.insertWith f link (Links links, [], paraIds) descendantMap' 
  
  pure thisMap--- $ thisPageAsMap <> map


-- | The Link is the Key
-- | DO NOT USE THIS FOR COMBINING MULTIPLE SCRAPE BATCHES!!! It may fail when it shouldn't  
siblingUnion' :: Link -> Payload' -> Payload' -> Payload'
siblingUnion' key x@(out, inn, pgId) y@(out', inn', pgId') = case (out == out' && pgId == pgId') of 
  False -> error "illegal data handling" -- <> (show key) <> (show x) <> (show y) -- out, paras should be equal
  True -> (out, inn <> (filter (\x -> not $ elem x inn) inn'), pgId')


-- | The Link is the Key
-- | DO NOT USE THIS FOR COMBINING MULTIPLE SCRAPE BATCHES!!! It may fail when it shouldn't  
siblingUnion :: Link -> Payload -> Payload -> Payload
siblingUnion key x@(out, inn, paras) y@(out', inn', paras') = case (out == out' && paras == paras') of 
  False -> error $ "illegal data handling" <> (show key) <> (show x) <> (show y) -- out, paras should be equal
  True -> (out, inn <> (filter (\x -> not $ elem x inn) inn'), paras)


-- left: parent, right: descendant 
parentUnion' :: Payload' -> Payload' -> Payload'
parentUnion' parent@(_, refsIn, _) child@(refsOut, refsInLower, paras)  = (refsOut, refsIn <> refsInLower {-allows duplication-}, paras)


-- left: parent, right: descendant 
parentUnion :: Payload -> Payload -> Payload
parentUnion parent@(_, refsIn, _) child@(refsOut, refsInLower, paras)  = (refsOut, refsIn <> refsInLower {-allows duplication-}, paras)

-- initializeGenre :: Genre -> IO (Genre, Link)
-- initializeGenre genre = do

-- | Hash of the Page data structure 
type PageID = Text

writePage :: Genre -> Depth -> Page -> IO ()
writePage genre depth page = do
  let pageId = show $ hmacGetDigest $ hmacAlg SHA256 "" $ toStrict $ Aeson.encode page   
  let dir = "/home/lazylambda/code/Ace/wikiResults/" <> (unpack genre) <> "/" <> (show depth) <> "/"
  let path = dir <> pageId <> ".json"
  createDirectoryIfMissing True dir
  Aeson.encodeFile path page


writeResult :: Genre -> Depth -> (ParagraphID, ResultOut) -> IO ()
writeResult genre depth (hashed, resultOut) = do
  let dir = "/home/lazylambda/code/Ace/wikiResults/" <> (unpack genre) <> "/" <> (show depth) <> "/"
  let path = dir <> hashed <> ".json"
  createDirectoryIfMissing True dir
  Aeson.encodeFile path resultOut


-- | We should also get Categories listed and external references 
-- | We could also track what genres Wikipedia has
  -- Categories on wikipedia could even be well-defined sub genres 

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  (_, html) <- getHtml mgr $ Link "https://en.wikipedia.org/wiki/Network_topology"
  
  
  

  -- let
  --   res :: Maybe [(Elem' String, [Link])]
  --   res = flip scrape html $ pTagWithLinks --  $ Link "https://en.wikipedia.org/wiki/Network_topology"

  -- I'll need to create a function initializeGenre
  --"https://en.wikipedia.org/wiki/Small"
  mappy <- runTreeMap' mgr "networking" (0, 2) (Link "https://en.wikipedia.org/wiki/Network_topology")
  mapM_ print $ Map.toList mappy 


  -- Technically I need to properly write this to the file system where I handle conflicts between JSON reps
  -- of a page

  print mappy 

--  Aeson.encodeFile mappy
  
  -- print label
  -- mapM_ print forest
  
  --print tree
  pure ()
  -- mapM_ print res 
  

  -- [Link] >>= \link -> mapM_ recursiveFunc n links

  -- - n : levels deep ; either approaches infinity or hits [] 
  -- - but at this point I am a leaf 

  -- - since i will quickly run into a memory overload as is, I can just have paragraphs be distinct IDs meaning the
  --   tree will be:

  --          type RecTree = Map Link ([ParaID], Map ParaID RecTree) -- Not all paragraphs will have links


  -- - then: this is all gonna be run at the top level as a forest ~ list

  -- successesM thisRecursiveFunction [@Genre]

  --TODO(galen): should i make successesParM
    
  -- end data structure
  

  -- TODO
      -- all text
      -- all links
      -- recursion into other pages 
  
  -- writeFile "results.txt" $ show $ maybe [] id tickerPrices


-- Only since we care about the links here, otherwise this would be the parser that ignores style tags 
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

  




