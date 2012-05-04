
Introduction
============

 This is a little program for practicing my skills on Haskell.
Is based on an example game of Land of Lisp, by Conrad Barski.
Consist of a repl, where the player sends commands to Cuco, the 
main character, for moving around in the world, looking ...

Data definition and main functions
==================================

  Here I define the module and import Data.List.

> module Main where
> import Data.List 
> import System.Console.Haskeline

  Now I define the main function, which initializes the game 
calling makeGameState and passing to repl, when repl ends it
says bye.

> main :: IO ()
> main = do repl makeGameState
>           return ()

  The repl function is recursive, it gets a state of play 
(GameState), do some IO and returns a modified state, which is
result of evaluating the player command over the parameter state.
The repl calls itself again with the modified state as parameter,
for making the game continuous until the player enters "quit".

> repl :: GameState -> IO GameState
> repl s = runInputT defaultSettings (loop s)
>           where 
>             loop :: GameState -> InputT IO GameState         
>             loop s = do
>                       outputStrLn $ (gamePrint s)  
>                       minput <- getInputLine "% "
>                       case minput of
>                             Nothing -> return s
>                             Just "quit" -> return s
>                             Just input -> loop 
>                                              (gameEval s 
>                                                     (words 
>                                                        (maybe "help" id minput)))
>                                               

  Now I define data structures for representing the world that 
Cuco is able to interact with. PlayerState is a list of lists of
String, each index is a Cuco's property, like the result message
of performing an action, its place in the world and the objects
he carry. There are some constants for making PlayerState indexes
more meaningful.

  A Node is a triple, that represents a vertex of a graph which
is the complete world. At first position it has the name of the
place. Second item is a list of triple of String which carries 
information about the edges, first is the name of the other 
adjacent place, second the way to get that place, and third
the direction (west, upstairs, ...). 

> type PlayerState = [[String]]
> type GameState = (PlayerState, [Node])
> type Node = (String, [(String,String,String)], [String])

> place = 0
> inventory = 1
> response = 2

Initializing the game
=====================
  
 These functions are constants, they define the state used by main,
the game map (a [Node]), and a banner to be used as initial
value of response. The player starts the game at the first place
of the game map.

> banner = ["******** Welcome to the wizard game ********"]
> 
> makeGameState :: GameState
> makeGameState = ([[mfst (head gmap)], [], banner], gmap)
>   where gmap = makeGameMap
>                  
> makeGameMap :: [Node]
> makeGameMap = [("living-room", [("attic", "upstairs", "ladder"), 
>                                 ("garden", "west", "door")], 
>                 ["wizard", "whiskey"]),
>                ("attic", [("living-room", "downstairs", "ladder")],
>                 ["welding torch"]),
>                ("garden", [("living-room", "east", "door")],
>                 ["bucket", "frog"])]

Defining interaction functions
==============================

 These functions are called "interaction" ones because the put a 
bridge between game commands and functions that perform their 
associated actions. 

  The gameEval function gets a state, a [String] and evaluates to
another state which is result of executing the action associated to
the command at head of [String] with the rest of the list as 
arguments. If the [String] is a singleton the argument will be the
current place. In case of unrecognized action the noaction function
is called. Currently majority of commands get two arguments.

> gameEval :: GameState -> [String] -> GameState
> gameEval state [] = help state ""
> gameEval state [x] = gameEval state [x,getCurrentPlace (fst state)]
> gameEval state [x,y] = (snd (maybe ("na" ,noaction) id  
>                              (value x (actions))))
>                        state y
> gameEval state xs = gameEval state (take 2 xs)
>
> value :: (Eq a) => a -> [(a,b)] -> Maybe (a,b)
> value key xs = find (\x -> (fst x) == key) xs
> 
> noaction :: GameState -> String -> GameState
> noaction st xs = setMessage st ("Invalid action on " ++ xs)
>
> actions :: [(String, (GameState -> String -> GameState))]
> actions = [("look", look), ("walk", walk), ("pick-up", pickUp), 
>            ("inventory", showInventory), ("drop", dropObject),
>            ("help", help)]

  The gamePrint function gets a state, does some formatting, and 
evaluates to a String result of executing some command.

> gamePrint :: GameState -> String
> gamePrint state =  mprint ((fst state) !! response)
>                   
>
> mprint :: [String] -> String
> mprint msgs = intercalate ".\n" msgs

Functions associated to game commands
=====================================

  These functions change the supplied state, are used to change 
Cuco's position, get information about Cuco is seeing, picking up
objects, etc. The parameter String is the object of the action, all
actions keep this signature for consistency.

  The walk function gets a String which is the place the player wants
 to move Cuco. That place must be adjacent to the current one. It
changes Cuco's position and the information about Cuco sees.
  
> walk :: GameState -> String -> GameState
> walk (a,b) xs = if (isAdjPlace b (getCurrentPlace a) xs) then
>                       (look ((setIndex a place [xs]), b) xs)
>                     else
>                   (setMessage (a,b) "Cannot go by that way")

  The look function gets information about the environment at the
supplied place as String.

> look :: GameState -> String -> GameState
> look (a,b) xs = setResponse (a,b) result
>   where result = if cplace /= defaultNode then
>                    (describeEdges cplace) ++ 
>                    [(describeObjects cplace)]
>                  else
>                    ["Not known place"]
>         cplace = findPlace b xs
>   
> defaultNode = ("", [("","","")], [""])        
>
> describeObjects :: Node -> String
> describeObjects node = enumSentence "I see: " (trd node)
>
> describeEdges :: Node -> [String]
> describeEdges node = map 
>                      (\edge -> "There is a/an " ++ (mfst edge) 
>                                ++ " going " ++ (msnd edge) ++ 
>                                " by the " ++ (trd edge))
>                      (msnd node)

  The pickUp function transfers an object from the current place to
the inventory, the object is supplied as a String. The dropObject
does the inverse action.
 
> pickUp :: GameState -> String -> GameState
> pickUp = objectTransfer addToIndex deleteObject 

> dropObject :: GameState -> String -> GameState
> dropObject = objectTransfer deleteAtIndex addObject 

  Action of making transference between inventory and node.

> objectTransfer :: (PlayerState -> Int -> String -> PlayerState) -> 
>                   (String -> Node -> Node) -> 
>                   GameState -> String -> GameState
> objectTransfer invAct ndAct (a,b) xs = 
>                if cplaceNode == changed
>                   then
>                      setMessage (a,b) "You cannot do that"
>                   else
>                      showInventory 
>                      (invAct a inventory xs, 
>                      unionBy (\x y -> (mfst x) == (mfst y)) 
>                      [changed] b) ""
>                      where changed = ndAct xs cplaceNode 
>                            cplaceNode = getCurrentPlaceNode (a,b)
 

  The showInventory function gets information about the objects that
Cuco carries.

> showInventory :: GameState -> String -> GameState
> showInventory (a,b) _ = setMessage (a,b) (enumSentence "I got: " 
>                                            (a !! inventory))

  The help function gets information about actions that Cuco can 
perform.

> help :: GameState -> String -> GameState
> help st _ = setMessage st (enumSentence "Commands :" 
>                            [k | (k,v) <- actions])


Utility functions
=================

  These functions are used by other as base.

  Finds the node associated to the place.

> findPlace :: [Node] -> String -> Node                 
> findPlace mp plc = maybe defaultNode id 
>                                 (find (\x -> plc == (mfst x))
>                                  mp)
  
  Gets the current place.

> getCurrentPlace :: PlayerState -> String
> getCurrentPlace x = head (x !! place)

  Gets the node given of the current place.

> getCurrentPlaceNode :: GameState -> Node
> getCurrentPlaceNode (a,b) = findPlace b (getCurrentPlace a)

  Gets adjacent places.                    
                      
> getAdjPlaces :: [Node] -> String -> [String]
> getAdjPlaces mp plc = map (\z -> mfst z)
>                       (msnd (findPlace mp plc))
  
  Says if a place is adjacent to another. 
  
> isAdjPlace :: [Node] -> String -> String -> Bool
> isAdjPlace mp a b = elem a (getAdjPlaces mp b)

  Sets an element at given index of a list.
                      
> setIndex :: [a] -> Int -> a -> [a]
> setIndex xs i e = take i xs ++ [e] ++ drop (i+1) xs
  
  Adds an element to a list pointed by its index.                  
                    
> addToIndex :: Eq a => [[a]] -> Int -> a -> [[a]]
> addToIndex = actionAtIndex addFirst
  
  Deletes an element of a list pointed by the index.             
               
> deleteAtIndex :: Eq a => [[a]] -> Int -> a -> [[a]] 
> deleteAtIndex = actionAtIndex delete 

  A base function for actions on a list of lists.
                  
> actionAtIndex :: Eq a => (a -> [a] -> [a]) -> [[a]] -> Int -> a 
>                  -> [[a]]
> actionAtIndex f xss i e = setIndex xss i (f e (xss !! i))

  Sets the response in a supplied state.
                            
> setResponse :: GameState -> [String] -> GameState
> setResponse (a,b) xs = (setIndex a response xs, b)

  Almost the same as setResponse but this one gets a String.
                         
> setMessage :: GameState -> String -> GameState
> setMessage st xs = setResponse st [xs]

  Deletes an object from the node.
                     
> deleteObject :: String -> Node -> Node
> deleteObject = actionObject delete                      

  Adds an object to the node.
                 
> addObject :: String -> Node -> Node
> addObject = actionObject addFirst 
  
  Base function for actions on node's objects.              
              
> actionObject :: (String -> [String] -> [String]) -> String ->  
>                 Node -> Node
> actionObject f obj node = (mfst node, msnd node, f obj (trd node))

  Creates a sentence of objects enumeration.
                            
> enumSentence :: String -> [String] -> String
> enumSentence xs xss = xs ++ intercalate "," xss
  
  Adds an item to the list at first position.                        
                        
> addFirst :: a -> [a] -> [a]
> addFirst x xs = x:xs

  Refer to elements of a triple.
                  
> trd (_, _, c) = c
> 
> mfst (a,_,_) = a
> 
> msnd (_,b,_) = b


