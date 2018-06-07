module TestIO
  ( announce
  , report
  , reportDoc
  ) where

import qualified Text.PrettyPrint.ANSI.Leijen   as Pp

announce :: String -> IO ()
announce s = do
  putStrLn ""
  putStrLn "----------------------------------------------------------------------"
  putStrLn $ "-- " ++ s

report :: String -> IO ()
report s = putStrLn $ "   " ++ s

reportDoc :: Pp.Doc -> IO ()
reportDoc = Pp.putDoc . Pp.indent 3
