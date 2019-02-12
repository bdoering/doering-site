{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe
import           Data.Monoid ((<>), mconcat)
import           Text.Pandoc 
import           Hakyll
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
postCtx = mconcat [ dateField "date" "%B %e, %Y"
                  , field "toc" $ \item ->
                      loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})
                  , mathCtx
                  , defaultContext
                  ]

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  mathjaxField <- getMetadataField (itemIdentifier item) "mathjax"
  return $ if isJust mathjaxField
           then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
           else ""

archiveCtx :: [Item String] -> Context String
archiveCtx ps =
  listField "posts" postCtx (return ps)
  <> constField "title" "Archives"
  <> defaultContext

indexCtx :: [Item String] -> Context String
indexCtx ps =
  listField "posts" postCtx (return ps)
    <> constField "title" "Home"
    <> defaultContext

--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------

static :: Rules ()
static = do
  match ("CNAME" .||.
         "favicon.ico" .||.
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
      ps <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
      let idxCtx = indexCtx ps
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
      ps <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
      let archiveCtx' = archiveCtx ps
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
         lastPosts <- fmap (take 20) . recentFirst =<<
                  loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
         renderAtom feedConfiguration feedCtx lastPosts

--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions :: Extensions
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        -- newExtensions = foldr S.insert defaultExtensions mathExtensions
        newExtensions = foldr enableExtension defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- See http://julien.jhome.fr/posts/2013-05-14-adding-toc-to-posts.html
pandocTocCompiler :: Compiler (Item String)
pandocTocCompiler =
    let writerOptions = defaultHakyllWriterOptions {
                          writerTableOfContents = True
                        , writerTemplate = Just "$toc$"
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

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
  -- postsToc
  archive
  templates
  otherPages
  atom

  match "posts/*" $ version "toc" $
        compile pandocTocCompiler
