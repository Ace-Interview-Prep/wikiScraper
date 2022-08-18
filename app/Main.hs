{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Scrappy.Requests
import Scrappy.Scrape
import Scrappy.Elem.ChainHTML
import Scrappy.Elem.SimpleElemParser
import Scrappy.Elem.Types (getHrefEl, innerHtmlFull, Elem', ElemHead, attrs, elTag)
import Scrappy.Links
import Scrappy.BuildActions (catEithers) -- TODO: Move to Scrappy.Types

import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Parsec (anyChar, (<|>), parse, many, try)
import Control.Applicative (liftA2)
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import Data.Text (Text, pack)
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
type Label = (ID_From, ThisLink, [ParagraphID]) 
runTree :: Manager -> OriginalGenre -> (Depth, MaxDepth) -> (ID_From, ThisLink) -> IO (Tree Label)
runTree mgr genre (depth,max) (id_from, link) = do

  -- Depth wouldn't actually affect the returned Tree just if it should stop
  -- ie case depth + 1 > threshold -> pure [] 
  
  (_, html) <- getHtml mgr link
  let
    res :: [(Elem' String, [LinkElem])]
    res = fromMaybe [] $ scrape pTagWithLinks html


    -- let links = fmap (getHrefEl True cLink) (catEithers eiths)

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

    -- should never fail, should never overwrite
    writeResult :: (ParagraphID, ResultOut) -> IO ()
    writeResult (hashed, resultOut) = do
      let dir = "/home/lazylambda/code/Ace/wikiResults/"
      let path = dir <> hashed <> ".json"
      createDirectoryIfMissing False dir
      Aeson.encodeFile path resultOut
    
  let paraIds = fmap fst signedReferencedParagraphs

  -- this should detect if depth is too much and then return pure [] in that case
  forest <- mapM (runTree mgr genre (depth+1, max)) labeledLinks

  let label = (id_from, link, paraIds)
--  Node label forest

  pure $ Node label forest 
  -- a link should have a Label such that Label <- where it was found : PTag + ThisLink


-- initializeGenre :: Genre -> IO (Genre, Link)
-- initializeGenre genre = do

  

--   pure (genre, link)


main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  (_, html) <- getHtml mgr $ Link "https://en.wikipedia.org/wiki/Network_topology"


  

  -- let
  --   res :: Maybe [(Elem' String, [Link])]
  --   res = flip scrape html $ pTagWithLinks --  $ Link "https://en.wikipedia.org/wiki/Network_topology"

  -- I'll need to create a function initializeGenre
  
  tree <- runTree mgr "topology" (0, 10) (mempty, Link "https://en.wikipedia.org/wiki/Network_topology")

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

  
