{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Yst.Build (buildSite) where

import           Control.Exception  (SomeException, catch)
import           Control.Monad
import           Data.List
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Data.Time.Calendar (Day (..))
import           Data.Time.Clock    (UTCTime (..), secondsToDiffTime)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO          (hPutStrLn, stderr)
import           Yst.Render
import           Yst.Types
import           Yst.Util


minTime :: UTCTime
minTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)


findSource :: Site -> FilePath -> IO FilePath
findSource = searchPath . sourceDir

dependencies :: Site -> String -> IO [FilePath]
dependencies site url =
  let
    page = fromMaybe
      (error $ "Tried to get dependencies for nonexistent page: " ++ url)
      (M.lookup url (pageIndex site))
  in do
  layout <- findSource site $ stripStExt (fromMaybe (defaultLayout site) $ layoutFile page) <.> "st"
  requires <- mapM (findSource site) $ requiresFiles page
  srcdir <- findSource site $
                 case sourceFile page of
                       TemplateFile f -> stripStExt f <.> "st"
                       SourceFile f   -> f
  let fileFromSpec (DataFromFile f _)      = Just f
      fileFromSpec (DataFromSqlite3 f _ _) = Just f
      fileFromSpec _                       = Nothing
  dataFiles <- mapM (searchPath $ dataDir site) $ mapMaybe (\(_,s) -> fileFromSpec s) $ pageData page
  return $ indexFile site : layout : srcdir : (requires ++ dataFiles)

filesIn :: FilePath -> IO [String]
filesIn dir = fmap (filter (/=".") . map (makeRelative dir)) (getDirectoryContentsRecursive dir)

buildSite :: Site -> IO ()
buildSite site = do
  files <- fmap concat $ mapM filesIn $ filesDir site
  let pages = M.keys $ pageIndex site
  let overlap = files `intersect` pages
  unless (null overlap) $ forM_ overlap
    (\f -> hPutStrLn stderr $ "Warning: the page '" ++ f ++ "' will overwrite the file by the same name.")
  forM_ files $ \file ->
     updateFile site file
  forM_ pages $ \page ->
     case M.lookup page (pageIndex site) of
          Just  pg -> updatePage site pg
          Nothing  -> error $ "Couldn't find page " ++ page

updateFile :: Site -> FilePath -> IO ()
updateFile site file = do
  let destpath = deployDir site </> file
  srcpath <- searchPath (filesDir site) file
  srcmod <- getModificationTime srcpath
  destmod <- catch (getModificationTime destpath)
                   (\(_::SomeException) -> return minTime)
  when (srcmod > destmod) $ do
    createDirectoryIfMissing True $ takeDirectory destpath
    hPutStrLn stderr $ "Updating " ++ destpath
    copyFile srcpath destpath

updatePage :: Site -> Page -> IO ()
updatePage site page = do
  let destpath = deployDir site </> pageUrl page
  deps <- dependencies site $ pageUrl page
  forM_ deps $ \dep -> do
    exists <- doesFileExist dep
    unless exists $ do
      hPutStrLn stderr $ "Missing dependency: " ++ dep
      hPutStrLn stderr $ "Aborting!  Cannot build " ++ destpath
      exitWith $ ExitFailure 3
  depsmod <- mapM getModificationTime deps
  destmod <- catch (getModificationTime destpath)
                   (\(_::SomeException) -> return minTime)
  when (maximum depsmod > destmod) $ do
    createDirectoryIfMissing True $ takeDirectory destpath
    hPutStrLn stderr $ "Updating " ++ destpath
    renderPage site page >>= writeFile destpath
