{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.
  
Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- getArgs :: IO (List Chars)
-- putStrLn :: Chars -> IO ()
-- readFile :: FilePath -> IO Chars
-- lines :: Chars -> List Chars
-- void :: IO a -> IO ()


-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile :: FilePath -> Chars -> IO ()
printFile fPath fContents = putStrLn fContents
-- printFile fPath fContents = putStrLn fContents

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles :: List (FilePath, Chars) -> IO ()
printFiles = void . sequence . (<$>) (uncurry printFile) 


-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile :: FilePath -> IO (FilePath, Chars)
getFile fPath  = pure (\x -> (fPath,x)) <*> (readFile fPath)
-- this is what the solutions say but i don't fully understand what's going on
-- getFile  = lift2 (<$>) (,) readFile 

-- -- Given a list of file names, return list of (file name and file contents).
-- -- Use @getFile@.
getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles files = sequence ((<$>) getFile files)

-- Given a file name, read it & for each line in that file, read, print contents of each.
-- Use @getFiles@ and @printFiles@.
run :: FilePath -> IO ()
run fName = printFiles =<< files -- IO ()
    where fileNames = pure lines <*> readFile fName -- IO (List Chars)
          files = getFiles =<< fileNames -- IO (List(FilePath,Chars)) 




  -- getArgs :: IO (List Chars)
  -- putStrLn :: Chars -> IO ()
  -- readFile :: FilePath -> IO Chars
  -- lines :: Chars -> List Chars
  -- void :: IO a -> IO ()
  --
-- Problem --
--   Given a single argument of a file name, read that file,
--   each line of that file contains the name of another file,
--   read the referenced file and print out its name and contents.

-- -- /Tip:/ use @getArgs@ and @run@
main :: IO ()
main = getArgs >>= (\args ->
                     case args of 
                       fileName :. Nil -> run fileName
                       _ -> putStrLn "usage: got to supply a filename"
                   )


----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
