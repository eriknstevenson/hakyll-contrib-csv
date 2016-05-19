{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CsvSpec where

import           Control.Monad
import           Data.ByteString.Lazy.Char8
import           Data.Csv
import           GHC.Generics
import           Hakyll.Contrib.Csv
import           Test.Hspec
import qualified Text.Blaze.Html.Renderer.String as H
import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

spec :: Spec
spec =
  describe "csvTable" $ do
    it "generates correct Html." $
      (H.renderHtml . csvTableContents $ testCsv) `shouldBe` H.renderHtml testHtml

data Person = Person {name :: !String, age :: !Int}
  deriving (Generic, Show)

instance ToNamedRecord Person
instance FromNamedRecord Person
instance DefaultOrdered Person

testCsv :: ByteString
testCsv = encodeDefaultOrderedByName testPeople

testHtml :: Html
testHtml = do
  H.thead $ H.tr $ do
    H.td $ H.toHtml ("name" :: String)
    H.td $ H.toHtml ("age" :: String)
  H.tbody $ forM_ testPeople $ \person ->
    H.tr $ do
      H.td $ H.toHtml $ name person
      H.td $ H.toHtml $ age person

testPeople :: [Person]
testPeople =
  [ Person "Bobby" 29
  , Person "Michelle" 34
  , Person "Rachael" 19
  ]