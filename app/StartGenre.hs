{-# LANGUAGE FlexibleContexts #-}

module StartGenre (startGenre) where

import Control.Monad.Trans.Except (runExceptT)
import Text.Parsec (ParsecT, Stream, space, anyChar, manyTill, letter, parse, noneOf)
import Scrappy.Links 
import Control.Applicative
import qualified Data.Map.Lazy as Map
import Data.Either
import Data.List
import Data.Maybe

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI.Encode (encode)

import Scrappy.Requests (getHtml)
import Scrappy.Elem.SimpleElemParser (el, elemParser)
import Scrappy.Elem.Types
import Scrappy.Scrape (scrape)
import Scrappy.JS (runVDOM)

type Title = String
--type Genre = String 


-- TODO(galen): move to scrappy 
getAttr :: ElementRep e => String -> e a -> Maybe String
getAttr atr e = Map.lookup atr $ attrs e  

startGenre :: Manager -> String -> IO Link
startGenre mgr genre = do
--  mgr <- newManager tlsManagerSettings
  -- let firstAttempt = Link "https://en.wikipedia.org/wiki/" <> encode genre
  -- (_,htmlMaybePage) <- getHtml mgr link

  -- -- check if the search term exists at all in the first paragraph
  -- let firstParagraph = 
  
  -- case 
  
  
  let link = ( Link $ "https://en.wikipedia.org/w/index.php?search="
               <> encode genre
               <> "&title=Special:Search&profile=advanced&fulltext=1&ns0=1"
             )
  (_,html) <- getHtml mgr link

 
  --Right html' <- runExceptT $ runVDOM link html 
  
  --writeFile "deleteme.html" html
--  print $ fromJust $ scrape (el "form" []) html
  --mapM_ print $ fromJust $ scrape (el "input" [("id", "ooui-php-1")]) html

  let target = elemParser (Just ["a"]) noPat [("href", Nothing),("title", Nothing), ("data-serp-pos", Nothing)]

  
  let optionLinks = catMaybes . fmap (\e -> (,) <$> getAttr "title" e <*> getHrefEl True link e) $ fromJust $ scrape target html

  pure . snd $ chooseLink genre optionLinks
  
  --pure $ innerHtmlFull . head . fromJust $ scrape (el "form" []) html 
  
--  pure $ Link "" --undefined
  --filter with "redirect from"

  
  
  -- I should try requesting the given term
  -- I will either
  --   get a page
  --     Can tell by the fact that the original term is not present in the first paragraph
  --       Good redirect / no redirect -> return this link 
  --       Bad redirect -> report 
         
         
       
  --   get search results
  --      Rank search results by what has the earliest same words

  --      example:
  --          America: History & Life

  --          Something that has America && History ranked above History + Life AND America + Life

-- instance Semigroup Link


-- instance Monoid Link where
--   mempty = Link "" 

  

chooseLink :: String -> [(Title,Link)] -> (Title, Link)
chooseLink g (o:opts) = foldr (maxScore g) o opts


upToWhich :: Int -> [String] -> String -> Int
upToWhich _ [] _ = 0
upToWhich mult (x:xs) str =
  if isInfixOf x str
  then (mult * 1) + upToWhich (mult - 1) xs str
  else upToWhich (mult - 1) xs str 

-- | TODO(galen): make sure that we are applying from the first to last result and prioritizing
-- | 
maxScore :: String -> (Title, Link) -> (Title, Link) -> (Title, Link)
maxScore genre one@(title1, link1) two@(title2, link2) =
  let
    genreWords = words' genre
    case1 = upToWhich (length genreWords) genreWords title1
    case2 = upToWhich (length genreWords) genreWords title2
    choose (c1, l1) (c2, l2) 
      | c1 > c2 = one 
      | c1 < c2 = two 
      | c1 == c2 = if length (words' title2) >= length (words' title1) then one else two 

  in
    choose (case1, length title1) (case2, length title2) 
    
    
words' :: String -> [String]
words' = fromRight [] . parse (some word) "" 

word :: Stream s m Char => ParsecT s u m String
word = do
  optional $ manyTill (noneOf (['A'..'Z'] <> ['a'..'z']))  space
  w <- some letter
  optional $ manyTill anyChar space
  pure w
  
  -- foldr (max . score) 




--data Radon = Radon Metadata1 AtomicComposition
