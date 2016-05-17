{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import Lucid
import Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "csv/*.csv" $ do
    route $ setExtension "html" `composeRoutes` gsubRoute "csv/" (const "")
    compile $ do
      table <- csvCompiler
      loadAndApplyTemplate "templates/default.html" defaultContext (fmap LBS.unpack table) >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

data CourseWare = CourseWare
  { courseID :: !Int
  , title    :: !String
  } deriving (Show)

instance FromNamedRecord CourseWare where
    parseNamedRecord r = CourseWare <$> r .: "id" <*> r .: "title"

csvCompiler :: Compiler (Item LBS.ByteString)
csvCompiler =
  getResourceLBS >>= withItemBody (csvToHtml)

csvToHtml :: LBS.ByteString -> Compiler LBS.ByteString
csvToHtml bs =
  case decodeByName bs of
    Left err -> return . LBS.pack $ err
    Right (_, rows) -> return . renderBS $
    --cellspacing_ "0"
      table_ [id_ "sortedTable", class_ "display", width_ "100%"] $ do
        thead_ $ tr_ $ do
          th_ "id"
          th_ "title"
        tbody_ $
          V.forM_ rows makeRow

makeRow :: CourseWare -> Html ()
makeRow row = tr_ $ do
  td_ $ toHtml . show . courseID $ row
  td_ $ toHtml . title $ row
