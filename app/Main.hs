{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Scrappy.Requests
import Scrappy.Scrape
import Scrappy.Elem.ChainHTML
import Scrappy.Elem.SimpleElemParser
import Scrappy.Elem.Types (getHrefEl, innerHtmlFull, Elem')
import Scrappy.Links
import Scrappy.BuildActions (catEithers) -- TODO: Move to Scrappy.Types

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Parsec (anyChar, (<|>), parse, many, try)
import Control.Applicative (liftA2)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.List

import Data.Map 

type Paragraph = Text
-- data RetraceableTree = RetraceableTree (Map Link ([Paragraph], RetraceableTree))
--   deriving (Semigroup, Monoid, Show)


type RetraceableTree = Map Link ([Paragraph], RetraceableTree)

type RetraceableTree' = Tree (Link [ElemHead])

-- uncurry(1)
-- >> Map Link ([Paragraph], Map Link 

type Depth = Int 

runTree :: (Depth, ElemHead, Link) -> IO RetraceableTree
runTree depth eh link = do

  -- Depth wouldn't actually affect the returned Tree just if it should stop
  -- ie case depth + 1 > threshold -> pure [] 
  
  html <- getHtml mgr link
  let res = scrape pTagWithLinks html


  let label :: (Link, [ElemHead])

  Node label forest

  - the problem with this is that what elemhead gave each branch of the forest
  - maybe I can use pTagWithLinks or generally, the link <a> tag to label each ( Link >>==> Forest )  

  forest <- mapM runTree links
  pure $ label forest 
  -- a link should have a Label such that Label <- where it was found : PTag + ThisLink



i could use FRP to model this

 - An output is some paragraph
 - two paragraphs dont necessarily relate here
 - but two paragraphs may be acted on by two other forces of change
   - in real terms, one set of  [paragraphs] ~ one page may be referenced by two different trees 



main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  (_, html) <- getHtml mgr $ Link "https://en.wikipedia.org/wiki/Network_topology"


  

  let
    res :: Maybe [(Elem' String, [Link])]
    res = flip scrape html $ pTagWithLinks $ Link "https://en.wikipedia.org/wiki/Network_topology"

  
  runLeaf

  [Link] >>= \link -> mapM_ recursiveFunc n links

  - n : levels deep ; either approaches infinity or hits [] 
  - but at this point I am a leaf 

  - since i will quickly run into a memory overload as is, I can just have paragraphs be distinct IDs meaning the
    tree will be:

           type RecTree = Map Link ([ParaID], Map ParaID RecTree) -- Not all paragraphs will have links


  - then: this is all gonna be run at the top level as a forest ~ list

  successesM thisRecursiveFunction [@Genre]

  --TODO(galen): should i make successesParM
    
  -- end data structure
  

  -- TODO
      -- all text
      -- all links
      -- recursion into other pages 
  
  mapM_ print res 
  
  -- writeFile "results.txt" $ show $ maybe [] id tickerPrices


-- Only since we care about the links here, otherwise this would be the parser that ignores style tags 
pTagWithLinks :: Link -> ScraperT (Elem' String, [Link])
pTagWithLinks cLink = do
  e <- el "p" []
  let txt = innerHtmlFull e
  eiths <- case parse f "in p tag" txt of
    Right x -> pure x 
    --Right (words, links) -> pure (e { innerHtmlFull = words }, links)
    Left _ -> error "idk ... weird" 


  let words = g eiths
  -- True because we'd like to stay on the same site 
  let links = fmap (getHrefEl True cLink) (catEithers eiths)

  pure (e { innerHtmlFull = words } , catMaybes links)             
      

  where
    g (x:xs) = case x of
      Right e -> innerHtmlFull e <> (g xs) 
      Left chr -> chr : (g xs)
    g [] = []
    
    f = many ((Right <$> (try $ el "a" [])) <|> (Left <$> anyChar))

  
