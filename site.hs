{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char
import           Data.Csv
import           Data.Monoid
import qualified Data.Text as T
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
    compile $ do
      table <- csvCompiler
      loadAndApplyTemplate "templates/default.html" (csvContext <> defaultContext) (fmap LBS.unpack table) >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

data CSVFile = CSVFile
  { courseID :: !Int
  , title    :: !String
  } deriving (Show)

instance FromNamedRecord CSVFile where
    parseNamedRecord r = CSVFile <$> r .: "id" <*> r .: "title"

csvCompiler :: Compiler (Item LBS.ByteString)
csvCompiler = do
  identifier <- T.pack . filterIdentifier <$> getUnderlying
  getResourceLBS >>= withItemBody (csvToHtml identifier)

csvToHtml :: T.Text -> LBS.ByteString -> Compiler LBS.ByteString
csvToHtml identifier bs =
  case decodeByName bs of
    Left err -> return . LBS.pack $ err
    Right (_, rows) -> return . renderBS $
    --cellspacing_ "0"
      table_ [id_ identifier, class_ "display", width_ "100%"] $ do
        thead_ $ tr_ $ do
          th_ "id"
          th_ "title"
        tbody_ $
          V.forM_ rows makeRow

makeRow :: CSVFile -> Html ()
makeRow row = tr_ $ do
  td_ $ toHtml . show . courseID $ row
  td_ $ toHtml . title $ row

csvContext = field "idname" $ return . filterIdentifier . itemIdentifier

filterIdentifier = filter (isLetter) . show

