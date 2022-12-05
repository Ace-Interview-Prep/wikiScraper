module Deprecated where


-- This module is all of the code ive written for this project that is still valuable but there exists a better

-- solution that may be yet untested



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


-- | ALTERNATE APPROACH
-- SIMPLE BIDIRECTIONAL MAP 
-- type SiteBidirectionalMap = Map.Map Link Payload
--type BidirectionalMap a (b :: * -> *) = Map a (b a) 
-- Note that this implies the whole internet could be mapped as




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



writeResult :: Genre -> Depth -> (ParagraphID, ResultOut) -> IO ()
writeResult genre depth (hashed, resultOut) = do
  let dir = "/home/lazylambda/code/Ace/wikiResults/" <> (unpack genre) <> "/" <> (show depth) <> "/"
  let path = dir <> hashed <> ".json"
  createDirectoryIfMissing True dir
  Aeson.encodeFile path resultOut



-- || DEPRECATED BECAUSE : Only using [RefIn] in State now instead, this has too much memory 

-- | TODO(galen): Have we handled how a leaf should look? such that
-- |              we know it hasnt been processed and can be continued on?
-- | TODO(galen): This doesnt show if the links found were none
--type Payload' = (RefsOut, [RefIn], PageID)
runTreeMapStateT :: Manager -> RefIn -> (Depth, MaxDepth) -> Link -> StateT BiDMap' IO ()
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
    --toStrict $ Aeson.encode wikiPage
    pageId = pack . show $ hmacGetDigest $ hmacAlg SHA256 "myKey" $ (show link)
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
      mapM_ (runTreeMapStateT mgr (Transient genre (depth) link) (depth, maxDepth)) links'
  pure () 
