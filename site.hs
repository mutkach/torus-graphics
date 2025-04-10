--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Debug.Trace
import Hakyll

import Hakyll.Images
  ( compressJpgCompiler,
    loadImage,
    resizeImageCompiler,
    scaleImageCompiler,
  )

import Hakyll.Web.Pandoc (defaultHakyllWriterOptions)
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss, tango, espresso, pygments, zenburn)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))

--------------------------------------------------------------------------------

pandocCodeStyle :: Style
pandocCodeStyle = breezeDark


myPandocCompiler' :: Compiler (Item String)
myPandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
        ,writerNumberSections = True
      }

main :: IO ()
main = hakyll $ do

    match "images/haskell-logo.png" $ do
        trace "fuck you1" $ return ()
        route idRoute

    match "images/*" $ do
        route   idRoute
        compile $ loadImage
                >>= scaleImageCompiler 1024 768

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        trace "fuck you" $ return ()
        >> makeItem $ styleToCss pandocCodeStyle

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
         myPandocCompiler'
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "projects/*" $ do
        route $ setExtension "html"
        compile $ do
         myPandocCompiler'
            >>= loadAndApplyTemplate "templates/project.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            projects <- recentFirst =<< loadAll "projects/*"
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "projects" postCtx (return projects) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            pure $ trace "fuck you"
            projects <- recentFirst =<< loadAll "projects/*"
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "projects" postCtx (return projects) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler




--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
