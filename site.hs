{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Text.Pandoc 
import           Control.Monad (forM_)
import           Hakyll
import qualified Data.Map as M
import qualified Data.Set as S
import           Abbreviations (abbreviationFilter)
    
--------------------------------------------------------------------
-- Text filters
--------------------------------------------------------------------

applyFilter :: (Monad m, Functor f) => (String -> String) -> f String -> m (f String)
applyFilter transformator str = return $ (fmap $ transformator) str


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
  match ("CNAME" .||.
         "assets/images/*" .||.
         "assets/ico/*" .||.
         "assets/js/*" .||.
         "assets/css/*" .||.
         "assets/fonts/*" .||.
         "assets/files/*" .||.
         fromRegex "^posts/.*/.*") $ do
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
    compile $ pandocMathCompiler
        >>= applyFilter abbreviationFilter
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= saveSnapshot "content"
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

atom :: Rules ()
atom = do
  create ["atom.xml"] $ do
    route idRoute
    compile $ do
         -- Overwrite existing description field with complete post
         -- let feedCtx = bodyField "description" <> postCtx
         let feedCtx = postCtx
         posts <- fmap (take 20) . recentFirst =<<
                  loadAllSnapshots "posts/*" "content"
         renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------

pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- compiler :: Compiler (Item String)
-- compiler = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

-- pandocOptions :: WriterOptions
-- pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

cfg :: Configuration
cfg = defaultConfiguration

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Björn Döring"
    , feedDescription = "A private blog"
    , feedAuthorName  = "Björn J. Döring"
    , feedAuthorEmail = "bjoern.doering@gmail.com"
    , feedRoot        = "http://doering.io"
    }

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
  atom
