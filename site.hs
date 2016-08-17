{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (pure, empty)
import           Data.Maybe
import           Data.Monoid (mappend, (<>), mconcat)
import           Text.Pandoc 
import           Control.Monad (forM_)
import           Hakyll
import qualified Data.Map as M
import qualified Data.Set as S
import           Abbreviations (abbreviationFilter)

import Hakyll.Core.Identifier
    
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

useTocCompiler_works item =
    let
        test f = if (isJust f)
                 then pure (error $ "no string value for bool field:")
                 else empty
    in do
      useTocField <- getMetadataField (itemIdentifier item) "useToc"
      return $ test useTocField

-- | Creates a 'field' to use with the @$if()$@ template macro.
boolField
    :: String
    -> (Item a -> Bool)
    -> Context a
boolField name f = field name (\i -> if f i
    then pure (error $ unwords ["no string value for bool field:",name])
    else empty)


-- field "speakers-names" (\conf -> getSpeakerNameCompiler lang conf) `mappend`



-- getSpeakerNameCompiler lang conf =
--     let getName (id, m) =
--             fromMaybe "" (M.lookup "firstname" m) ++ " " ++
--             fromMaybe "" (M.lookup "lastname" m)
--     in do
--       speakerList <- getSpeakerList conf
--       speakers <- getSpeakers lang speakerList
--       return $ intercalate (", ") $ map getName speakers




--    (M.member "toc") (getMetadata (itemIdentifier item))

--isJust $ M.lookup "toc" (getMetadata (itemIdentifier item)) -- do
              -- metadata <- getMetadata (itemIdentifier item)
              -- -- isJust (M.lookup "toc" metadata)
              -- False

-- \item -> ("toc"::String) `M.member` (getMetadata (itemIdentifier item))

-- getToc id = do
--   metadata <- getMetadata id
--   if isJust (M.lookup "toc" metadata)
--   then missingField -- return ""
--   else loadBody (id { identifierVersion = Just "toc"})
                
-- getRoomClass id = do
-- md <- getMetadata id
-- return $ case (M.lookup "location" md) of
-- Just "Room 1" -> "event-maxi"
-- Just "Room 2" -> "event-mini"
-- Just "Trempolino" -> "event-trempolino"
-- _ -> ""
-- getConstRoomClassCtx id =
-- field "eventcolor" (const (getRoomClass id))
-- roomClassCtx = field "eventcolor" (getRoomClass . itemIdentifier)

-- field "eventcolor" (getRoomClass . itemIdentifier) `mappend`



-- tocCtx :: Context String
-- tocCtx
--     | "toc" `M.member` metadata
--     | otherwise = missingField
--     where
--       metadata <- getMetadata $ itemIdentifier item

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
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
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

postsToc :: Rules ()
postsToc = do
  match "posts/*" $ version "toc" $
        compile pandocTocCompiler

archive :: Rules ()
archive = do
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
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
                  loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
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

-- See http://julien.jhome.fr/posts/2013-05-14-adding-toc-to-posts.html
pandocTocCompiler =
    let writerOptions = defaultHakyllWriterOptions {
                          writerTableOfContents = True
                        , writerTemplate = "$toc$"
                        , writerStandalone = True
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
  -- postsToc
  archive
  templates
  otherPages
  atom

  match "posts/*" $ version "toc" $
        compile $ pandocCompilerWith defaultHakyllReaderOptions
                defaultHakyllWriterOptions {
                          writerTableOfContents = True
                        , writerTemplate = "$toc$"
                        , writerStandalone = True
                        }
