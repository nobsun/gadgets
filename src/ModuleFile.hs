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

initTestModule :: ModuleName -> ModuleName -> IO ()
initTestModule templ modname = do
    { let { modfile  = ("test" </>)  $ (`addExtension` "hs")
                     $ foldr1 (</>) $ splitOn "." modname
          ; tmplfile = ("test" </>)  $ (`addExtension` "hs")
                     $ foldr1 (</>) $ splitOn "." templ
          ; moddir   = takeDirectory modfile
          }
    ; createDirectoryIfMissing True moddir
    ; bool (filterFile (convname' templ modname) tmplfile modfile)
           (error $ "initTestModule: module file already exists: " ++ modfile)
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

convname' :: ModuleName -> ModuleName -> String -> String
convname' m n s = case break (unwords ["module", m] `isPrefixOf`) (lines s) of
    (_:xs,_:ys)    -> case break (unwords ["import", m'] `isPrefixOf`) ys of
        (ps,_:qs)      -> unlines $ ("-- # " ++ n) : (xs ++ ("module " ++ n) : (ps ++ unwords ["import", n'] : qs))
        _           -> error "invalid template module"
    _           -> error "invalid template module"
    where
        m' = reverse (drop 4 (reverse m))
        n' = reverse (drop 4 (reverse n))
