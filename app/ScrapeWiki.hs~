{-# LANGUAGE OverloadedStrings #-}

module ScrapeWiki (scrapeWikiPage, writePage) where 

import Types 
 
import Scrappy.BuildActions (catEithers) -- TODO: Move to Scrappy.Types
import Scrappy.Elem
import Scrappy.Links
import Scrappy.Elem.Types
import Scrappy.Scrape -- TODO(galen): move ScraperT to types

import System.Directory (createDirectoryIfMissing)
import qualified Data.Aeson as Aeson 
import Text.Parsec (anyChar, (<|>), parse, many, try)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (pack, strip, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.List





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
    (links, Page link genInst (paras $ fmap fst res) links references categories')



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


    -- I could use contains here with the shell of mw-parser-output if it was the efficient impl. TBCompleted
    f = many ((Right <$> (try $ elemParserWhere (Just ["a"]) noPat "href" (isPrefixOf "/wiki/")))
               <|> (Left <$> anyChar)
             )



writePage :: Genre -> Depth -> Link -> Page -> IO ()
writePage genre depth link page = do
  let pageId = toHashPath link
  let dir = "/home/lazylambda/code/Ace/wikiResults/" <> (unpack genre) <> "/" <> (show depth) <> "/"
  let path = dir <> pageId <> ".json"
  createDirectoryIfMissing True dir
  Aeson.encodeFile path page


