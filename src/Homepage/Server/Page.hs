module Homepage.Server.Page (
  renderHtml,
) where

import Data.ByteString.Lazy (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (for_)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Homepage.Server.Page.Data (PageData (..), StaticAssets (..))
import qualified Homepage.Server.Page.Data as PageData
import qualified Text.Blaze as Blaze
import Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

fromText :: Coercible a Text => a -> Blaze.AttributeValue
fromText =
  Blaze.textValue . coerce

fromFilePath :: FilePath -> Blaze.AttributeValue
fromFilePath =
  Blaze.stringValue

searchTab :: Bool -> PageData.Search -> Html
searchTab isActive PageData.Search{..} = do
  let tabClasses
        | isActive = "tab tab--active"
        | otherwise = "tab"

  H.li ! A.class_ tabClasses ! A.title (fromText searchName) ! Blaze.dataAttribute "href" (fromText searchHref) $
    H.button ! A.class_ "tab__button" ! A.type_ "button" $
      H.img ! A.class_ "tab__icon" ! A.alt (fromText searchName) ! A.src (fromFilePath searchImage)

searches :: Text -> [PageData.Search] -> Html
searches searchIcon groups =
  H.header ! A.class_ "search" $ do
    H.ol ! A.class_ "search__tabs" $
      for_ (zip [0 :: Int ..] groups) $ \(index, search) ->
        searchTab (index == 0) search

    H.div ! A.class_ "search__form" $
      H.form ! A.id "search-form" ! A.class_ "search-bar" $ do
        let placeHolder =
              "Search "
                <> case PageData.searchName . NonEmpty.head <$> nonEmpty groups of
                  Nothing -> ""
                  Just name -> " " <> fromText name

        H.input
          ! A.class_ "search-bar__input"
          ! A.type_ "text"
          ! A.name "search"
          ! A.autocomplete "off"
          ! A.placeholder placeHolder

        H.button ! A.class_ "search-bar__submit-button" ! A.type_ "submit" ! A.title "Execute search" $
          H.preEscapedText searchIcon

linkList :: PageData.LinkGroup -> Html
linkList PageData.LinkGroup{..} =
  H.div ! A.class_ "link-group" $ do
    H.h1 ! A.class_ "link-group__name" $ H.toHtml (PageData.fromName linkGroupName)

    H.ul ! A.class_ "link-group__links" $
      for_ linkGroupLinks $ \PageData.Link{..} ->
        H.li $
          H.a ! A.class_ "link" ! A.href (fromText linkHref) $
            H.toHtml (PageData.fromName linkName)

linkGroups :: [PageData.LinkGroup] -> Html
linkGroups groups =
  H.main ! A.class_ "links" $
    for_ groups linkList

renderHtml :: StaticAssets -> PageData -> ByteString
renderHtml StaticAssets{..} PageData{..} = Utf8.renderHtml . H.docTypeHtml $ do
  H.head $ do
    H.title "New  Tab"

    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"

    H.style $ H.preEscapedText (coerce baseStyles)

    for_ pageStyleSheets $ \href ->
      H.link ! A.rel "stylesheet" ! A.href (fromFilePath href)

    for_ pageScripts $ \href ->
      H.script ! A.defer "defer" ! A.src (fromFilePath href) $ mempty

  H.body $ do
    searches searchIcon pageSearches
    linkGroups pageLinkGroups

    H.script $ H.preEscapedText (coerce baseScript)
