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
import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

-- |A Hakyll 'compiler' used to build a plain Html table from Csv data.
csvCompileTable :: Compiler (Item String)
csvCompileTable = getResourceLBS >>= withItemBody (return . H.renderHtml . csvTable)

-- |Generates Blaze Html data from a lazy bytestring of Csv data.
csvTable :: LBS.ByteString -> Html
csvTable bs =
  case decode NoHeader bs of
    Left err -> do
      H.h1 "Error parsing CSV file!"
      H.p $ H.toHtml err
    Right rows ->
      H.table $ do
        H.thead $ makeRow (V.head rows)
        H.tbody $ forM_ (V.tail rows) makeRow

-- |A Hakyll 'compiler' that builds only the inner part of an Html table from Csv data.
-- Typically inserted into '<table>' tags with a hakyll template.
csvCompileTableContents :: Compiler (Item String)
csvCompileTableContents = getResourceLBS >>= withItemBody (return . H.renderHtml . csvTableContents)

-- |Generates Blaze Html data representing the table without outer '<table>' tags.
-- This is useful when used in conjuction with a template to add additional properties to the table.
csvTableContents :: LBS.ByteString -> Html
csvTableContents bs =
  case decode NoHeader bs of
    Left err -> do
      H.h1 "Error parsing CSV file!"
      H.p $ H.toHtml err
    Right rows -> do
      H.thead $ makeRow (V.head rows)
      H.tbody $ forM_ (V.tail rows) makeRow

makeRow :: Vector BS.ByteString -> Html
makeRow row = H.tr $ forM_ row $ \column -> H.td $ H.toHtml . BS.unpack $ column

