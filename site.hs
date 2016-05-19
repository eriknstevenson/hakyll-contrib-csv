{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char
import           Data.Csv
import           Data.Monoid
import qualified Data.Text.Lazy as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Lucid
import           Hakyll

main :: IO ()
main = hakyll $ do

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "csv/*.csv" $ do
    route $ setExtension "html" `composeRoutes` gsubRoute "csv/" (const "")
    compile $
      fmap (T.unpack . renderText) <$> csvCompiler
      >>= loadAndApplyTemplate "templates/layout.html" (csvContext <> defaultContext)
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler


csvCompiler :: Compiler (Item (Html ()))
csvCompiler = do
  identifier <- makeTableID <$> getUnderlying
  getResourceLBS >>= withItemBody (csvToHtml identifier)

csvToHtml :: String -> LBS.ByteString -> Compiler (Html ())
csvToHtml identifier bs =
  case decode NoHeader bs :: Either String Csv of
    Left err -> return mempty
    Right rows -> return $
      table_ [id_ (T.toStrict . T.pack $ identifier), class_ "display", width_ "100%"] $ do
        thead_ $
          makeRow (V.head rows)
        tbody_ $
          V.forM_ (V.tail rows) makeRow

makeRow :: Vector ByteString -> Html ()
makeRow row = tr_ $ V.forM_ row $ \column -> td_ $ toHtml . BS.unpack $ column

csvContext = field "table-id" $ return . makeTableID . itemIdentifier

makeTableID = filter isLetter . show

