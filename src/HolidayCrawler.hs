{-# LANGUAGE OverloadedStrings #-}

module HolidayCrawler
  ( getHolidays
  , Country(..)
  , Year
  , Region(..)
  , Holiday(..)
  , HolidayType(..)
  , formatDay
  ) where

import Control.Lens
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Calendar
import qualified Network.Wreq as R
import Text.HTML.TagSoup
import Data.Time

openURL :: Text -> IO Text
openURL x = do
  r <- R.get $ T.unpack x
  return $ T.decodeUtf8 $ BSL.toStrict $ r ^. R.responseBody

data HolidayType
  = Official
  | Festive
  | Event
  deriving (Show, Eq, Ord)

data Region
  = AllRegions
  | Regions [Text]
  deriving (Show)

data Holiday = Holiday
  { holidayDay :: Day
  , holidayHolidayType :: HolidayType
  , holidayName :: Text
  , holidayRegion :: Region
  } deriving (Show)

instance Eq Holiday where
  Holiday {holidayDay = day1, holidayHolidayType = holidayType1} == Holiday { holidayDay = day2
                                                                            , holidayHolidayType = holidayType2
                                                                            } =
    (day1, holidayType1) == (day2, holidayType2)

instance Ord Holiday where
  Holiday {holidayDay = day1, holidayHolidayType = holidayType1} `compare` Holiday { holidayDay = day2
                                                                                   , holidayHolidayType = holidayType2
                                                                                   } =
    (day1, holidayType1) `compare` (day2, holidayType2)

data Country
  = Germany
  | Suisse
  | Austria

type Year = Int

countryUrlPath :: Country -> Text
countryUrlPath Germany = "deutschland"
countryUrlPath Austria = "oesterreich"
countryUrlPath Suisse = "schweiz"

rowType :: HolidayType -> Text
rowType Official = "gesetzlich_row"
rowType Festive = "nicht_gesetzlich_row"
rowType Event = "ereignis_row"

getHolidays :: Country -> Year -> [HolidayType] -> IO [Holiday]
getHolidays country year holidayTypes = do
  src <- htmlSource country year
  let tags = parseTags src
  let holidayPartitions =
        L.foldl
          (\acc holidayType -> acc ++ rowPartitions tags holidayType)
          []
          holidayTypes
  return $ L.sort $ L.map holiday holidayPartitions
  where
    rowPartitions :: [Tag Text] -> HolidayType -> [(HolidayType, [Tag Text])]
    rowPartitions tags holidayType =
      L.map (\tags' -> (holidayType, tags')) $
      partitions
        (~== TagOpen
               ("tr" :: Text)
               [("class", T.concat ["row_panel ", rowType holidayType])])
        tags
    holiday :: (HolidayType, [Tag Text]) -> Holiday
    holiday (holidayType, tags') = Holiday day holidayType name region
      where
        day =
          parseDate $
          T.unpack $
          L.last $
          T.words $
          innerText $
          L.take
            2
            (L.dropWhile (~/= ("<div>" :: String)) $
             L.dropWhile
               (~/= TagOpen ("td" :: Text) [("class", "feiertag_datum")])
               tags')
        name =
          T.unwords $
          T.words $
          innerText $ L.take 2 $ L.dropWhile (not . isTagOpenName "a") tags'
        region =
          case (country, regionTokens) of
            (Germany, ["alle", "BL"]) -> AllRegions
            (Austria, ["landesweit"]) -> AllRegions
            (Suisse, ["landesweit"]) -> AllRegions
            (_, []) -> AllRegions
            _ -> Regions regionTokens
        regionTokens =
          T.words $
          innerText $
          L.take 2 $
          L.dropWhile
            (not .
             hasAttr ("class", "feiertag_responsive_small sf_w_xs sf_tooltip"))
            tags'

--  return $ show $ gesetzlichRowPartitions
hasAttr :: (Text, Text) -> Tag Text -> Bool
hasAttr attr (TagOpen _ xs) = attr `L.elem` xs
hasAttr _ _ = False

intToText :: Int -> Text
intToText = T.pack . show

htmlSource :: Country -> Year -> IO Text
htmlSource country year =
  openURL $
  T.concat
    [ "https://www.schulferien.org/"
    , countryUrlPath country
    , "/feiertage/"
    , intToText year
    ]

parseDate :: String -> Day
parseDate = parseTimeOrError True defaultTimeLocale dayFormatGerman

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale dayFormatGerman

dayFormatGerman :: String
dayFormatGerman = "%d.%m.%Y"
