module AoC.Core.Fetcher where

import AoC.Core.Day
import AoC.Prelude
import Control.Exception
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client
import Network.HTTP.Req as Req
import Network.HTTP.Types.Status
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import Web.Cookie (SetCookie (..), defaultSetCookie)

fetchDay :: String -> Day -> IO ()
fetchDay year day = do
  fp <- getDataFileName $ "input/" <> displayDayFile day
  alreadyFetched <- doesFileExist fp
  when alreadyFetched $
    printError $ "Input file for " <> year <> "-12-" <> displayDay day <> " already fetched"
  session <- getEnv "AOC_SESSION"
  now <- getCurrentTime
  bs <-
    fetchFile year session now day
      `catch` (errorWithoutStackTrace . handleException)
  writeFile fp (T.unpack bs)
  printSuccess "Successfully fetched input file"

fetchFile :: String -> String -> UTCTime -> Day -> IO Text
fetchFile year session now day = do
  let host = "adventofcode.com"
      url =
        Req.https (T.pack host)
          /: T.pack year
          /: "day"
          /: T.pack (show (getDay day))
          /: "input"
  bs <-
    Req.runReq Req.defaultHttpConfig $
      Req.req Req.GET url Req.NoReqBody Req.bsResponse $
        Req.cookieJar (mkSessionCookie host session now)
  pure $ decodeUtf8 $ Req.responseBody bs

handleException :: Req.HttpException -> String
handleException (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException rsp _)))
  | responseStatus rsp == notFound404 = "Day not (yet?) found"
handleException e = show e

mkSessionCookie :: String -> String -> UTCTime -> CookieJar
mkSessionCookie host session now =
  createCookieJar $
    maybeToList $
      generateCookie
        defaultSetCookie
          { setCookieName = "session",
            setCookieValue = BS.pack session
          }
        defaultRequest {host = BS.pack host}
        now
        True
