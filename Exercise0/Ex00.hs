module Ex00 where

name, idno, username :: String
name      =  "Hamza Mughees"  -- replace with your name
idno      =  "17329860"    -- replace with your student id
username  =  "mugheesh"   -- replace with your TCD username


declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

{- Modify everything below here as you see fit to ensure all tests pass -}

hello  =  "Hello World :-)"
