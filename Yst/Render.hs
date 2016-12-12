{-# LANGUAGE OverloadedStrings #-}
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

module Yst.Render (renderPage) where

import           Control.Monad       (forM)
import           Data.Char           (toLower)
import           Data.List           (intercalate)
import           Data.List.Split     (wordsBy)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (pack)
import           Data.Text.Lazy      (unpack)
import           Data.Time           (getCurrentTime, utctDay)
import           Lucid
import           System.Directory    (canonicalizePath)
import           System.FilePath     (takeExtension)
import           Text.Pandoc         hiding (Format)
import           Text.Pandoc.Error   (handleError)
import           Text.StringTemplate

import           Yst.Data
import           Yst.Types
import           Yst.Util


-- | @relUrl a b@ returns a URL for @b@ relative to @a@.  So, for
-- example, @relUrl "a" "a/b.html" = "b.html"@,
-- @relUrl "" "a/b.html" = "a/b.html"@, and @relUrl "a" "b.html" = "../b.html"@
relUrl :: String -> String -> String
relUrl relto url = intercalate "/" $ relPath ++ [urlBase]
  where relPath = relPaths reltoPaths urlPaths
        (reltoPaths, urlPaths) = dropCommon (wordsBy (=='/') relto) (wordsBy (=='/') urlDir)
        urlBase = takeUrlBase url
        urlDir = takeUrlDir url

takeUrlBase :: String -> String
takeUrlBase = reverse . takeWhile (/= '/') . reverse

takeUrlDir :: String -> String
takeUrlDir = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse

relPaths :: [String] -> [String] -> [String]
relPaths [] ys     = ys
relPaths (_:xs) ys = ".." : relPaths xs ys

dropCommon :: (Eq a) => [a] -> [a] -> ([a],[a])
dropCommon (x:xs) (y:ys) | x == y = dropCommon xs ys
dropCommon xs ys         = (xs,ys)

renderNav :: String -> [NavNode] -> String
renderNav targeturl nodes = unpack $ renderText $
  ul_ [class_ "nav tree"] $ mapM_ (renderNavNode targeturl) nodes

renderNavNode :: String -> NavNode -> Html ()
renderNavNode targeturl (NavPage tit pageurl) =
  li_ [class_ "current" | pageurl == targeturl] (a_ [href_ pageurl'] (toHtml tit))
    where targetdir = takeUrlDir targeturl
          pageurl' = pack $ relUrl targetdir pageurl
renderNavNode targeturl (NavMenu tit nodes) = li_ [] $
    do a_ [class_ "tree-toggle nav-header"] (toHtml tit)
       ul_ [class_ "nav tree"] (mapM_ (renderNavNode targeturl) nodes)
    where active = targeturl `isInNavNodes` nodes
          isInNavNodes u = any (isInNavNode u)
          isInNavNode u (NavPage _ u') = u == u'
          isInNavNode u (NavMenu _ ns) = u `isInNavNodes` ns

formatFromExtension :: FilePath -> Format
formatFromExtension f =
  case map toLower $ takeExtension f of
    ".html"     -> HtmlFormat
    ".xhtml"    -> HtmlFormat
    ".latex"    -> LaTeXFormat
    ".tex"      -> LaTeXFormat
    ".context"  -> ConTeXtFormat
    ".1"        -> ManFormat
    ".rtf"      -> RTFFormat
    ".texi"     -> TexinfoFormat
    ".db"       -> DocBookFormat
    ".fodt"     -> OpenDocumentFormat
    ".txt"      -> PlainFormat
    ".markdown" -> PlainFormat
    _           -> HtmlFormat

renderPage :: Site -> Page -> IO String
renderPage site page = do
  let menuHtml = renderNav (pageUrl page) (navigation site)
  let layout = fromMaybe (defaultLayout site) $ layoutFile page
  srcDirs <- mapM canonicalizePath $ sourceDir site
  gs <- mapM directoryGroupRecursive srcDirs
  let g = foldl1 mergeSTGroups gs
  attrs <- forM (pageData page) $ \(k, v) -> getData site v >>= \n -> return (k,n)
  todaysDate <- fmap utctDay getCurrentTime
  let root' = case length (filter (=='/') $ pageUrl page) of
                    0 -> []
                    n -> concat $ replicate n ("../" :: String)
  rawContents <-
    case sourceFile page of
          SourceFile sf   -> fmap (filter (/='\r')) $ searchPath srcDirs sf >>= readFile
          TemplateFile tf -> do
            templ <- getTemplate tf g
            return $ render
                    . setManyAttrib attrs
                    . setAttribute "root" root'
                    . setAttribute "gendate" todaysDate
                    $ templ
  layoutTempl <- getTemplate layout g
  let format = formatFromExtension (stripStExt layout)
  let contents = converterForFormat (pageOptions page) format rawContents
  return $ render
         . setManyAttrib attrs
         . setAttribute "sitetitle" (siteTitle site)
         . setAttribute "pagetitle" (pageTitle page)
         . setAttribute "gendate" todaysDate
         . setAttribute "contents" contents
         . setAttribute "root" root'
         . setAttribute "nav" menuHtml
         $ layoutTempl

converterForFormat :: WriterOptions -> Format -> String -> String
converterForFormat wrOpts f =
  let reader = handleError . readMarkdown def { readerSmart = True } in
  case f of
    HtmlFormat         -> writeHtmlString htmlOpts . reader
    LaTeXFormat        -> writeLaTeX wrOpts . reader
    PlainFormat        -> id
    ConTeXtFormat      -> writeConTeXt wrOpts . reader
    ManFormat          -> writeMan wrOpts . reader
    RTFFormat          -> writeRTF wrOpts . reader
    DocBookFormat      -> writeDocbook wrOpts . reader
    TexinfoFormat      -> writeTexinfo wrOpts . reader
    OpenDocumentFormat -> writeOpenDocument wrOpts . reader
  where
    htmlOpts = wrOpts
      { writerHtml5      = True
      , writerHighlight  = True
      , writerStandalone = True
      , writerTemplate   = "<div>$toc$</div><div>$body$</div>"
      }

getTemplate :: Stringable a => String -> STGroup a -> IO (StringTemplate a)
getTemplate templateName templateGroup =
  let
    template = fromMaybe
      (error $ "Could not load template: " ++ templateName)
      (getStringTemplate (stripStExt templateName) templateGroup)
  in do
  case checkTemplate template of
    (Just parseErrors, _, _ ) ->
      errorExit 17 $ "Error in template '" ++ templateName ++ "': " ++ parseErrors
    (_, _, Just templatesNotFound) ->
      errorExit 21 $ "Templates referenced in template '" ++ templateName ++ "' not found: " ++ intercalate ", " templatesNotFound
    (_, _, _) ->
      return ()
  return template
