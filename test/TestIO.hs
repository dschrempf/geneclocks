module TestIO
  ( announce
  , report
  , reportText
  , reportDoc
  ) where

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Text.PrettyPrint.ANSI.Leijen as Pp

announce :: String -> IO ()
announce s = do
  putStrLn ""
  -- putStrLn "----------------------------------------------------------------------"
  putStrLn $ "-- " ++ s

report :: String -> IO ()
report s = putStrLn $ "   " ++ s

reportText :: T.Text -> IO ()
reportText t = T.putStrLn $ T.pack "   " <> t

reportDoc :: Pp.Doc -> IO ()
reportDoc = Pp.putDoc . Pp.indent 3
