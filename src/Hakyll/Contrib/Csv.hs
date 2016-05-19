-- | This module implements translation of CSV files to Blaze
-- HTML for use with the Hakyll static site compiler.

{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Contrib.Csv ( csvCompileTable
                          , csvTable
                          , csvCompileTableContents
                          , csvTableContents) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Csv
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Hakyll
import qualified Text.Blaze.Html.Renderer.String as H
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

csvCompileTable :: Compiler (Item String)
csvCompileTable = getResourceLBS >>= withItemBody (return . H.renderHtml . csvTable)

csvTable :: LBS.ByteString -> Html
csvTable bs =
  case decode NoHeader bs of
    Left err -> do
      H.h1 $ "Error parsing CSV file!"
      H.p $ H.toHtml err
    Right rows ->
      H.table ! A.class_ "display" ! A.width "100%" $ do
        H.thead $ makeRow (V.head rows)
        H.tbody $ forM_ (V.tail rows) makeRow

csvCompileTableContents :: Compiler (Item String)
csvCompileTableContents = getResourceLBS >>= withItemBody (return . H.renderHtml . csvTableContents)

csvTableContents :: LBS.ByteString -> Html
csvTableContents bs =
  case decode NoHeader bs of
    Left err -> do
      H.h1 $ "Error parsing CSV file!"
      H.p $ H.toHtml err
    Right rows -> do
      H.thead $ makeRow (V.head rows)
      H.tbody $ forM_ (V.tail rows) makeRow

makeRow :: Vector BS.ByteString -> Html
makeRow row = H.tr $ forM_ row $ \column -> H.td $ H.toHtml . BS.unpack $ column



