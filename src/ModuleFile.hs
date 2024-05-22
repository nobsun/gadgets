module ModuleFile where

import Data.Bool
import Data.List
import Data.List.Split
import System.Directory
import System.FilePath
import System.IO

type ModuleName = String

initModule :: ModuleName -> ModuleName -> IO ()
initModule templ modname = do
    { let { modfile  = ("src" </>)  $ (`addExtension` "hs")
                     $ foldr1 (</>) $ splitOn "." modname
          ; tmplfile = ("src" </>)  $ (`addExtension` "hs")
                     $ foldr1 (</>) $ splitOn "." templ
          ; moddir   = takeDirectory modfile
          }
    ; createDirectoryIfMissing True moddir
    ; bool (filterFile (convname templ modname) tmplfile modfile)
           (error $ "initModule: module file already exists: " ++ modfile)
           =<< doesFileExist modfile
    }

filterFile :: (String -> String) -> FilePath -> FilePath -> IO ()
filterFile conv src dst
    = withFile src ReadMode  (\ sh ->
      withFile dst WriteMode (\ dh ->
        hPutStr dh . conv =<< hGetContents sh ))

convname :: ModuleName -> ModuleName -> String -> String
convname m n s = case break (unwords ["module", m] `isPrefixOf`) (lines s) of
    (_:xs,_:ys) -> unlines $ (("-- # " ++ n) :) $ xs ++ ("module " ++ n) : ys
    _           -> error "invalid template module"