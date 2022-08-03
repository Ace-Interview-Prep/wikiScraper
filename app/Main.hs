module Main where

import Scrappy.Requests
import Scrappy.Scrape
import Scrappy.Elem.ChainHTML
import Scrappy.Elem.SimpleElemParser

main :: IO ()
main = do
  html <- getHtml' "https://dexscreener.com/"
  let tickerPrices =
        flip scrape html $ 
        (el "a" [("class", "chakra-link css-1oo4dn7")])
        `contains`
        (el "span" [("class", "chakra-text css-njkgh9")] </>>= (el "span" [("class", "css-1k4xub7")]))

  writeFile "results.txt" $ show $ maybe [] id tickerPrices
