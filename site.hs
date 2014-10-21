--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Text.Pandoc 
import           Control.Monad (forM_)
import           Hakyll
import qualified Data.Map as M
    
--------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  <> mathCtx
  <> defaultContext

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ if "mathjax" `M.member` metadata
           then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
           else ""

archiveCtx posts =
  listField "posts" postCtx (return posts)
  <> constField "title" "Archives"
  <> defaultContext

indexCtx posts =
  listField "posts" postCtx (return posts)
    <> constField "title" "Home"
    <> defaultContext

--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------

static :: Rules ()
static = do
  match ("assets/images/*" .||.
         "assets/ico/*" .||.
         "assets/js/*" .||.
         "assets/fonts/*" .||.
         "assets/files/*") $ do
           route idRoute
           compile copyFileCompiler
  
index :: Rules ()
index = do
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let idxCtx = indexCtx posts
      getResourceBody
        >>= applyAsTemplate idxCtx
        >>= loadAndApplyTemplate "templates/default.html" idxCtx
        >>= relativizeUrls 

less :: Rules ()
less = do
  -- No matter which less file changes, always recompile the same file
  match "less/*.less" $ do
    route . constRoute $ "assets/css/bdoering.css"
    compile $ getResourceString >>=
      withItemBody (unixFilter "lessc" ["less/bdoering.less"]) >>=
      return . fmap compressCss
      >>= relativizeUrls

posts :: Rules ()
posts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

archive :: Rules ()
archive = do
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx' = archiveCtx posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx'
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx'
        >>= relativizeUrls

templates :: Rules ()
templates = do
  match "templates/*" $ compile templateCompiler

otherPages :: Rules ()
otherPages = do
  let src = ["about.md"]
  match (fromList src) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------

compiler :: Compiler (Item String)
compiler = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

cfg :: Configuration
cfg = defaultConfiguration

--------------------------------------------------------------------
-- Main
--------------------------------------------------------------------
      
main :: IO ()
main = hakyllWith cfg $ do
  index
  static
  less
  posts
  archive
  templates
  otherPages
  




