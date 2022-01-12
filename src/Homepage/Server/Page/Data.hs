module Homepage.Server.Page.Data (
  Name (..),
  Href (..),
  Link (..),
  LinkGroup (..),
  Search (..),
  PageData (..),
  StaticAssets (..),
) where

import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)

data StaticAssets = StaticAssets
  { baseStyles :: Text
  , baseScript :: Text
  , searchIcon :: Text
  }

newtype Name = Name
  { fromName :: Text
  }

instance Aeson.FromJSON Name where
  parseJSON = fmap Name . Aeson.parseJSON

newtype Href = Href
  { fromHref :: Text
  }

instance Aeson.FromJSON Href where
  parseJSON = fmap Href . Aeson.parseJSON

data Link = Link
  { linkName :: Name
  , linkHref :: Href
  }

instance Aeson.FromJSON Link where
  parseJSON = Aeson.withObject "Link" $ \object ->
    Link
      <$> object .: "name"
      <*> object .: "href"

data LinkGroup = LinkGroup
  { linkGroupName :: Name
  , linkGroupLinks :: [Link]
  }

instance Aeson.FromJSON LinkGroup where
  parseJSON = Aeson.withObject "LinkGroup" $ \object ->
    LinkGroup
      <$> object .: "name"
      <*> object .: "links"

data Search = Search
  { searchName :: Name
  , searchImage :: FilePath
  , searchHref :: Href
  }

instance Aeson.FromJSON Search where
  parseJSON = Aeson.withObject "Search" $ \object ->
    Search
      <$> object .: "name"
      <*> object .: "image"
      <*> object .: "href"

data PageData = PageData
  { pageSearches :: [Search]
  , pageLinkGroups :: [LinkGroup]
  , pageStyleSheets :: [FilePath]
  , pageScripts :: [FilePath]
  }

instance Aeson.FromJSON PageData where
  parseJSON = Aeson.withObject "PageData" $ \object ->
    PageData
      <$> object .:? "searches" .!= []
      <*> object .:? "linkGroups" .!= []
      <*> object .:? "stylesheets" .!= []
      <*> object .:? "scripts" .!= []
