{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad (foldM_, forM_, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString.UTF8 (toString)
import Data.Default (def)
import Data.Map (Map, singleton, elems,  insert)
import Data.QRCode
import Data.Word (Word8)
import Graphics.GD
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Parse
import System.Environment (getArgs)
import System.Random
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (head, form, title, map)
import Text.Blaze.Html5.Attributes hiding (id)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H

-- The QR-Code is a matrix of 0/1 bytes
type QRCodeMatrix = [[Word8]]

-- Partition a list into a matrix with a leading value
partitionLeading :: Int -> a -> [a] -> [[a]]
partitionLeading w l xs = map nthPartition [0,w..(length xs)] where
    nthPartition n = take w $ snd (splitAt n xs) ++ repeat l

-- Transform a list of zeros and ones into a hexadecimal string
toHex :: [Word8] -> String
toHex = map hexChar . partitionLeading 4 0 where
    hexChar [0,0,0,0] = '0'
    hexChar [0,0,0,1] = '1'
    hexChar [0,0,1,0] = '2'
    hexChar [0,0,1,1] = '3'
    hexChar [0,1,0,0] = '4'
    hexChar [0,1,0,1] = '5'
    hexChar [0,1,1,0] = '6'
    hexChar [0,1,1,1] = '7'
    hexChar [1,0,0,0] = '8'
    hexChar [1,0,0,1] = '9'
    hexChar [1,0,1,0] = 'A'
    hexChar [1,0,1,1] = 'B'
    hexChar [1,1,0,0] = 'C'
    hexChar [1,1,0,1] = 'D'
    hexChar [1,1,1,0] = 'E'
    hexChar [1,1,1,1] = 'F'
    hexChar x = error $ "Wrong input to hexChar: " ++ show x

-- Transforms a hexadecimal string into a list of zeros and ones
fromHex :: String -> [Word8]
fromHex = concatMap hexList where
    hexList '0' = [0,0,0,0]
    hexList '1' = [0,0,0,1]
    hexList '2' = [0,0,1,0]
    hexList '3' = [0,0,1,1]
    hexList '4' = [0,1,0,0]
    hexList '5' = [0,1,0,1]
    hexList '6' = [0,1,1,0]
    hexList '7' = [0,1,1,1]
    hexList '8' = [1,0,0,0]
    hexList '9' = [1,0,0,1]
    hexList 'A' = [1,0,1,0]
    hexList 'B' = [1,0,1,1]
    hexList 'C' = [1,1,0,0]
    hexList 'D' = [1,1,0,1]
    hexList 'E' = [1,1,1,0]
    hexList 'F' = [1,1,1,1]
    hexList x = error $ "Wrong input to hexList: " ++ show x

-- Transform a qr-code matrix into an hex string
matrixToHex :: QRCodeMatrix -> String
matrixToHex = toHex . concat

-- Transform an hex string into a qr-code matrix, given the width and height
matrixFromHex :: Int -> Int -> String -> QRCodeMatrix
matrixFromHex w h = partitionLeading w 0 . take (w*h) . fromHex

-- | Same as 'strip', but applies only to the left side of the string.
lstrip :: String -> String
lstrip [] = []
lstrip s@(x:xs) = if x `elem` " \t\r\n" then lstrip xs else s

-- | Same as 'strip', but applies only to the right side of the string.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- Utility function to convert from a lazy to strict byte-string
toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

-- Utility function to convert from a strict to lazy byte-string
fromStrict :: B.ByteString -> BL.ByteString
fromStrict s = BL.fromChunks [s]

-- Return a random permutation from the list. Implementation based on
-- http://okmij.org/ftp/Haskell/perfect-shuffle.txt
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen (k:ks) =
    toElems $ foldl doStep (initial k gen) $ zip [1..] ks where
        toElems (x, y) = (elems x, y)
        initial x g = (singleton 0 x, g)
        doStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
        doStep (m, g) (e, x) = ((insert j x . insert e (m M.! j)) m, g') where
            (j, g') = randomR (0, e) g

-- From a list of friends names, return a random assignment of gift giver and
-- receiver, as tuples, given that the receiver name will be encoded as a
-- QR-Code. The assignment is such that all giver-receiver are in a single
-- loop.
assign :: RandomGen g => g -> [String] -> IO ([(String, QRCodeMatrix)], g)
assign g friends@(_:_:_) = do
    qrCodes <- mapM qrEncode $ tail shuffled ++ shuffled
    return $ shuffle g' $ zip shuffled qrCodes where
        (shuffled, g') = shuffle g friends
        qrEncode s = liftM toMatrix $
                     encodeString s Nothing QR_ECLEVEL_L QR_MODE_EIGHT True
assign _ _ = error "Please provide at least two names"

defaultLayout :: Html -> Html
defaultLayout bodyContent = docTypeHtml $ do
    H.head $ do
        H.title "Secret Santa"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/normalize.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/main.css"
    body bodyContent

-- Render the user input form HTML
userForm :: Html
userForm = defaultLayout $ do
        h1 "Secret Santa"
        H.form ! method "POST" ! acceptCharset "UTF-8" $ do
            p "Write the participants names (one per line), after that, press \"send\" button"
            textarea ! name "friends" ! rows "15" ! cols "30" $ ""
            br
            input ! type_ "submit" ! value "send"

-- Render the result (friends list + qr-codes images) HTML
userResult :: [(String, QRCodeMatrix)] -> Html
userResult friends = defaultLayout $ do
        p ! class_ "info" $ do
           strong "Instructions: "
           "Print this page and cut on the dotted lines. \
           \Distribute each cut paper to the participant with the displayed name. \
           \The \"secret friend\" is \"hidden\" in the QR-Code \
           \(one must use a QR-Code reader to know it's secret friend)."
        ul $ forM_ friends renderFriend where
            renderFriend (friend, codeMatrix) = li $ do
                    let w = show $ length codeMatrix
                        h = show . length $ head codeMatrix
                        imgUrl =  w ++ "/" ++ h ++ "/" ++ matrixToHex codeMatrix ++ ".png"
                    img ! src (toValue imgUrl)
                    p $ toHtml friend

-- Render a QR-Code data into PNG bytes
renderQRCode :: QRCodeMatrix -> IO BI.ByteString
renderQRCode qrMatrix = do
    let white = rgb 255 255 255
        black = rgb 0 0 0
        qrCell = 5 -- size, in pixels, of the qr-code cell
    image <- newImage (qrCell * length (head qrMatrix),
                       qrCell * length qrMatrix)
    fillImage white image
    let
        renderLine x line = do
            foldM_ (renderRect x) 0 line
            return $ x + qrCell
        renderRect _ y 0 = return $ y + qrCell
        renderRect x y _ = do
            drawFilledRectangle (y, x) (y+qrCell, x+qrCell) black image
            return $ y + qrCell
    foldM_ renderLine 0 qrMatrix
    savePngByteString image

-- display user form for input
handle :: Application
handle Request {requestMethod = "GET", pathInfo = []} = return $
    responseLBS status200 [("Content-Type", "text/html; charset=UTF-8")] $
                renderHtml userForm

-- display the list with the qr-codes
handle request@(Request {requestMethod = "POST", pathInfo = []}) = do
    (params, _) <- parseRequestBody lbsBackEnd request
    case params of
        [("friends", text)] -> do
            let friends = map rstrip $ lines $ toString $ urlDecode True text
            g <- liftIO newStdGen
            (codes, _) <- liftIO $ assign g friends
            return $ responseLBS status200
                [("Content-Type", "text/html; charset=UTF-8")] $
                renderHtml $ userResult codes
        _ -> forbidden

-- display the QR-Code image
handle Request {requestMethod = "GET", pathInfo = [w, h, baseName]}
    | ".png" `T.isSuffixOf` baseName = do
        let qrcodeHex = T.takeWhile (/= '.') baseName
        imageData <- liftIO $ renderQRCode $ matrixFromHex
             (read $ T.unpack w) (read $ T.unpack h) (T.unpack qrcodeHex)
        return $ responseLBS status200 [("Content-Type", "image/png")] $
                 fromStrict imageData
    | otherwise = notFound

-- Static file: favicon.ico
handle Request {requestMethod = "GET", pathInfo = ["favicon.ico"]} = return $
    ResponseFile status200 [("Content-Type", "image/icon")]
                 "static/favicon.ico" Nothing

-- Static file: robots.txt
handle Request {requestMethod = "GET", pathInfo = ["robots.txt"]} = return $
    ResponseFile status200 [("Content-Type", "text/plain")]
                 "static/robots.txt" Nothing

-- Static file: CSS and Javascript files
handle Request {requestMethod = "GET", pathInfo = ["static", baseName]} = return $
    ResponseFile status200 [("Content-Type", contentType baseName)]
                 ("static/" ++ T.unpack baseName) Nothing where
                     contentType fn
                        | ".css" `T.isSuffixOf` fn = "text/css"
                        | ".js" `T.isSuffixOf` fn = "application/javascript"
                        | otherwise = "application/octet-stream"

-- The default handler: show a 404 page
handle _ = notFound

notFound :: ResourceT IO Response
notFound = return $
      responseLBS status404 [("Content-Type", "text/plain")] "Not found"

-- The forbidden (401) page
forbidden :: ResourceT IO Response
forbidden = return $
    responseLBS status401 [("Content-Type", "text/plain")] "Forbidden"

-- Start the server on the default port (3000) or one specified in the command
-- line.
main :: IO ()
main = do
    args <- getArgs
    let port = case args of
            x:_ -> read x
            _ -> 3000
    run port . gzip def . logStdout . autohead $ handle

