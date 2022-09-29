
REQUIREMENTS!
  - Must be able to identify exactly how we arrived at said body of text
  - Must be able to tell when there's an overlapping trajectory ~~ intersectTrees :: A -> B -> GenredTree
  - Must be able to scrape for a while without memory malfunction
     - therfore: must output data chunks to system asap
        - therefore no two paragraphs should 

-- A paragraph ID must be a hash of the paragraph text
  - So that we can recover it from the filesystem
  - And the URL is in order to recover where the source of the text was

SO!
  a valid program here should allow for this:

  Look at text, query tree for its filepath hash / Paragraph ID 

  With Paragraph ID, find it's url AND be able to `step` up the scraped tree all the way up to the genre

  --> WE should label the text with a genre and have it in a genre folder

      cat results/<genre>/<hash>
      >> { "genre": "physics", "text": "The neils bohr diagram has been ...." }

      AND <hash> == hashSha256 "The neils bohr diagram has been ...."

  

WHAT IS THIS USED FOR:
  - Each body of text should be processed so that we can create two outputs
    1. the Sentence tree -> has this been said?
       is: A forest of sentences 
    2. \x-> Genred Sets =<< stanford NLP $ x
      -> of Verbs
      -> of Nouns
      -> of Abjectives

WHAT IS THIIIS USED FOR:


  f newSentence where f :: Text -> (GenreProbabilities, GrammaticalValidity (is exists), Support (times stated)) 



Node label (Forest label) 
  where
    label = (ID_From, ThisLink, [ParagraphIDs]) 
    -- this should work because the link on which the ElemHead (ID_From) exists, is in the label of the Forest

type ID_From = ElemHead

leaf == Node (_, "http://some wiki page", ["3rd43d", "ded32de33", "e32d32d"]) []



- If i do make my own datatype then I can add conditionality so that a Leaf may be continued
  although that is a lot of work when I can just do this


  - the problem with this is that what elemhead gave each branch of the forest
  - maybe I can use pTagWithLinks or generally, the link <a> tag to label each ( Link >>==> Forest )  

  

  - I probably just need a way to label the link so that its not detached
   - its not as if this function couldnt re-attach it but why would i

  - The core principle of what this func does is create a tree of links

  Link {{ [ Element.link ]] && paragraphs >>= output -> link {{

  - the actions to do the next step are imperative while the data structure is FRPish
   - it may be fair to say it has to be imperative cuz we have no clue what the next step may be
   - except at a high level... although this could be enough for FRP

  - SO!!!!!!!
    - Do some action (however we cut it up)
    - label it in some context
    - make it FRP ish in terms of combining the leaves 



i could use FRP to model this

 - An output is some paragraph
 - two paragraphs dont necessarily relate here
 - but two paragraphs may be acted on by two other forces of change
   - in real terms, one set of  [paragraphs] ~ one page may be referenced by two different trees 



--- SUMMARIZATION MODEL:


-- | Behaves like a tree except that it may contain a reference to a more
-- | real branch. This is a non-duplicating tree that may have multiple references to the same node.
data ConditionTree a = CNode a [ConditionTree a]
                     | Reference [a]

-- | b is probably a concrete type that just explains how to access the node

mkNonDuplicating :: Tree a -> ConditionTree a b
mkNonDuplicating (Node a (node:nodes)) = undefined
  -- how this could work is by trying to reach each leaf of a node and if along the way, for a specific query (or if optimized set of queries determined to not overlap)
  -- that it finds a match, it will analyze


When merging a tree into the ConditionTree,

1) First try the top node, and search through the entirety of the destination tree
2) If this does not work, then we need to track the upper components
  --> OR begin moving the Node over, except it has no nodes yet

3) continue with the forest and one by one try to match tree to some node
  --> NOTE: simplification: We never need to change what's already been placed in the new tree

(when working on a given node, we eventually place it in the new tree under its parent as either (Ref | NodeC with no children yet) )



type ConditionForest a = [ConditionTree a] 


-- | ALTERNATE APPROACH
-- SIMPLE BIDIRECTIONAL MAP 
Map Link (RefsOut, [Link], [Paragraph])

type BidirectionalMap a (b :: * -> *) = Map a (b a) 

data RefsOut = Links [Link]
             | EmptyLeaf -- Literal dead end
             | RichLeaf -- We purposely chose to stop here. The scraper should not scrape until infinity before going to another term.
                        -- arguably, this should take very small steps eg. do 5 depth for each term / RichLeaf then cycle to the next 

data RefIn = Start Genre -- The link is the Key itself 
           | Transient Genre Link Depth

-- this implies for:

findStartingPoints :: BidirectionalMap Link (,Text) -> [Link]
findStartingPoints = undefined
   -- Just try all nodes for RefIns isNull 


depth :: [RefIn] -> NominalDepth

meanDepth :: [RefIn] -> NominalDepth -- excludes outliers 

rationalDepth --> the concept of deciphering between disconnection and real grounding
   --> For example: if there are 9 entries / RefIn's and average RefIn's is 0.0001 then this is significant regardless,
       , it then becomes interesting how deep each entry is
   --> But if there is 1 entry and its depth of 50000 then this is niche, but not fundemental

       --> Since this is a Map now, in order to find the depth of the tree absolutely we'd have to calculate this but A) oh well and B) there may not be many cases like this

Depth is an important factor for only genre-analysis for
1) Showing strength of connection to an arbitrary genre
2) Relating genres to each other ... which kinda serves to validate the term or not


-- | Due to the design of this 
discretize :: Forest a -> ConditionForest a
discretize forest = foldr ([] :: ConditionForest a) foldTreeFunc forest 
  
  --ct : (go t ct) : foldTreeFunc

  where

    foldTreeFunc :: Tree a -> ConditionForest a -> CondtionForest a 
    foldTreeFunc tree (tc:forest) = case nodeA == nodeB of
      True -> case depthT > depthTC of
        True -> rebuildConditionNodeWith nodeA
        False -> 

          
      False -> recurseInto tc 
    
  
   -- this could track the path like :: [a] 



compareNode :: (a -> Tree a) -> a -> Tree a -> Tree a 
compareNode func x (Node l forest) = Node l' $ (<>) forest' $ case x == l of
  True -> Node l (func forest) -- in our case this is more so the problem at hand
  False -> fmap (compareNode x) forest


A better strategy in this case is to write this with a Forest as an argument to work on and then it just fmaps the effect of a tree

the only other problem is I have to get the params from the (left) side of the tree and work sideways


exampleTree = Node "a"
              [Node "b" []
              , Node "c" [Node "a" []]
              , Node "d" [Node "a" []]
              ]


1) check that we have a leaf (of our working copy of the tree) (we will delete this leaf when we find no copy of it from our working tree and add it to our new tree as it was)
2) Compare leaf to siblings
3) IF no match in siblings, ask parent for aunts and uncles
  4) Compare with aunts and uncles
  5) Compare with cousins
  6) If no match in cousins, delete from the cousins from the temp tree and continue on
     7) Ask grandparents for equality as well as the grandparents siblings, mapping to what could be a forest - then unfold their other branch of family and either work from top to bottom or bottom back up, looking for equal nodes

<at any point if an equality exists, the query should end, this *matching* node will be resumed when we get to it>

<it also doesn't matter whether a node has a massive descendance or [], this is a matter of unfolding via scrapers>

< so if we find a match, we are just replacing a node in the end tree as a referential node > <<although what if its the second node that should change?>>
     8)



If I model this as a fold then at any given time, I'm just checking if the current Node is in the tree

- I can maybe eliminate time by `elem this $ mconcat . levels` logic



- WE can act on the simplifying law that any Equivalent Node Labels have the same Forest in them
  - So if we work from top down, this is the cleanest for sure cuz bottom up could lead to unncecessary work
    that we eventually erase, higher up
    --> this applies to both sides

  - SO we should start with the left tree

    - Top node L <---> Top node R `when` True -> the ::b retains the parents but child is now a ref
    - Top node L <---> Top forest R -- this is still just labels tho (?)
    - Top node L <---> forests of top Forest ......

    No? Ok continue: Add the Node, but continue to evaluate its forest before re-building it on
                     the new tree

    - SndLvl node L <---> Top node R
    ....


  - But since the function is (a -> b -> b) we can act on the 'b' either by placing the Node/Tree or
    or placing a Ref which will evaluate to our new Node of the tree
    - This is true if we have the information of the conflicting node


  - Because we can work from a top-down, it follows that we can do shit like:

     established <> f xs ... eg: 1 : 2 : 3 : f xs

  *** The name for this is a SetTree 

  Which in Trees translates to:

     Node establishedLabel $ treeGood1 (<- evalTree) : f trees



So whats a neat recursive comparative algorithm?

  
foldTree :: Tree a -> Map k v -> Map k v
foldTree 


transformTree :: Tree a -> ConditionTree a
transformTree init =
  let
    copy = init
    endTree = compareNode 
  in
    endTree 


as i am building the tree itself I have a table of every single URL with its position 
  


instance Semigroup (ConditionTree a b) where












--------------------------------------------------------=-----------------------------------------------------------



Correlation of Construct table:
- Should either be evaluated at the paragraph or page level
- Or could just use this datatype:

  data WrittenContiguity = Page
                         | Paragraph
                         | Sentence


and this will go through a Stream<WrittenContiguity> of words 


And build a Map Word [(WrittenContiguity, Word)] where any word can be keyed to see that it had a relation of Page || Paragraph || Sentence to each word on the page

LAW:
  So \word -> lookup word theMap >>= \((contig, word2):words) -> lookup word2 theMap >>= \words -> pure $ elem word words

  SHOULD EQUAL: Just True or the implementation is invalid

  So this is a bidirectional map 

-- not just head tho, all elements

Then we merge {{  type PageContiguity = Map Word [(WrittenContiguity, Word)] }}

with the same maps except maybe:

  Map Word [(X, Word)] where data X = { page :: Count
                                      , paragraph :: Count
                                      , sentence :: Count
                                      }

 At this point we shouldn't need to worry about the bidirectional equivalence and can just fold each key of the map
 into this new map


  


3 core models
- Sentence , Grammatical
- Contiguity
- Genre of a given Term


Contiguity will allow us to expand on the Genre analysis
+ Genre should include the nature of the tree


















  
