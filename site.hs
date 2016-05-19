{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char
import           Data.Csv
import           Data.Monoid
import qualified Data.Text.Lazy as T
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
      ((fmap . fmap) (T.unpack . renderText) $ csvCompiler)
      >>= (\table -> loadAndApplyTemplate "templates/layout.html" (csvContext <> defaultContext) table)
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

data CSVFile = CSVFile
  { courseID :: !Int
  , title    :: !String
  } deriving (Show)

instance FromNamedRecord CSVFile where
    parseNamedRecord r = CSVFile <$> r .: "id" <*> r .: "title"

csvCompiler :: Compiler (Item (Html ()))
csvCompiler = do
  identifier <- filterIdentifier <$> getUnderlying
  getResourceLBS >>= withItemBody (csvToHtml identifier)

csvToHtml :: String -> LBS.ByteString -> Compiler (Html ())
csvToHtml identifier bs =
  case decodeByName bs of
    Left err -> return mempty
    Right (_, rows) -> return $
    --cellspacing_ "0"
      table_ [id_ (T.toStrict . T.pack $ identifier), class_ "display", width_ "100%"] $ do
        thead_ $ tr_ $ do
          th_ "id"
          th_ "title"
        tbody_ $
          V.forM_ rows makeRow

makeRow :: CSVFile -> Html ()
makeRow row = tr_ $ do
  td_ $ toHtml . show . courseID $ row
  td_ $ toHtml . title $ row

csvContext = field "table-id" $ return . filterIdentifier . itemIdentifier

filterIdentifier = filter isLetter . show

