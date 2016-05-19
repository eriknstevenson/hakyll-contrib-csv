{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Csv
import qualified Data.Text.Lazy as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Lucid
import           Hakyll

main :: IO ()
main = hakyll $ do

  match "csv/*.csv" $ do
    route $ setExtension "html" `composeRoutes` gsubRoute "csv/" (const "")
    compile $
      fmap (T.unpack . renderText) <$> csvCompiler
      >>= loadAndApplyTemplate "templates/layout.html" defaultContext
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

csvCompiler :: Compiler (Item (Html ()))
csvCompiler = getResourceLBS >>= withItemBody csvToHtml

csvToHtml :: LBS.ByteString -> Compiler (Html ())
csvToHtml bs =
  case decode NoHeader bs of
    Left _ -> return mempty
    Right rows -> return $
      table_ [class_ "display", width_ "100%"] $ do
        thead_ $ makeRow (V.head rows)
        tbody_ $ V.forM_ (V.tail rows) makeRow
  where
    makeRow :: Vector ByteString -> Html ()
    makeRow row = tr_ $ V.forM_ row $ \column -> td_ $ toHtml . BS.unpack $ column


