import System.Environment
import Control.Monad.Error
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.List
import Data.Word
import Data.List.Split (splitOn)
import Network.BSD
import Network.URI
import qualified Data.Text as T
import Test.WebDriver
import Test.WebDriver.Commands

helpMsg :: IO ()
helpMsg = putStrLn "usage: shrinkray URL element_id"

commonTags :: [String]
commonTags = ["br", "div", "style", "html", "footer", "script", "meta",
                "title", "center", "align", "span", "li", "ul", "head"]

commonCoords :: [(Word, Word)]
commonCoords = [(320, 1000), (480, 1000), (768, 1000), (992, 1000), (1200, 1000)]

data Args = Args {url :: Maybe String, elementId :: Maybe String } deriving (Show)

defaultArgs = Args Nothing Nothing

parseArgs :: [(String, Int)] -> Args -> Args
parseArgs [] args = args
parseArgs ((s,0):xs) args = parseArgs xs (args { url = Just s })
parseArgs ((s,1):xs) args = parseArgs xs (args { elementId = Just s})
parseArgs _ args = args

findConflicts :: (Word, Word) -> Element -> [Element] -> WD [String]
findConflicts (cx,cy) e [] = return []
findConflicts (cx,cy) e (x:xs) = do
    aPos <- elemPos e
    aSize <- elemSize e
    aTagName <- tagName e
    aId <- attr e (T.pack "id")
    bPos <- elemPos x
    bTagName <- tagName x
    let filterTag = (T.unpack bTagName)
    bId <- attr x (T.pack "id")
    if aTagName == bTagName || aId == bId || filterTag `elem` commonTags
    then findConflicts (cx,cy) e xs
    else
        let xMax = max (fst aPos) (fst bPos)
            xMin = min (fst aPos) (snd bPos)
            yMax = max (snd aPos) (snd bPos)
            yMin = min (snd aPos) (snd bPos)
            xThreshold = (fromIntegral ((fst aSize) + 1) :: Int)
            yThreshold = (fromIntegral ((snd aSize) + 1) :: Int)
            foundConflict = (xMax - xMin) <= xThreshold && (yMax - yMin) <= yThreshold
        in if foundConflict
            then do
                let cMsg = "Intrusion upon " ++ 
                           (T.unpack (fromMaybe (T.pack "") aId)) ++ " from "
                           ++ (T.unpack (fromMaybe (T.pack "") bId)) ++ 
                           " for Window Size " ++ show cx ++ "x" ++ show cy ++
                           ". Intruding element tag type: " ++ filterTag 
                           ++ " position: x: " ++ show (fst bPos) ++ " y: " 
                           ++ show (snd bPos) 
                conflict <- findConflicts (cx,cy) e xs
                return (cMsg : conflict)
            else findConflicts (cx,cy) e xs

shrinker :: Args -> (Word, Word) -> WD [String]
shrinker a (x,y) = do
    let eId = elementId a
    setWindowSize (x,y)
    openPage (fromMaybe "" (url a))
    rootElement <- findElem (ByXPath (T.pack "/*"))
    element <- findElem (ById (T.pack (fromMaybe "" eId)))
    allElements <- findElemsFrom rootElement (ByCSS (T.pack "*"))
    findConflicts (x,y) element allElements
 
openSite :: Args -> IO ()
openSite a = do
        myhost <- getHostName
        runSession defaultConfig { wdCapabilities = defaultCaps { browser = chrome } } $ do
            setImplicitWait 30000
            openPage (fromMaybe "" (url a))
            results <- mapM (shrinker a) $ reverse commonCoords
            let flatResults = concat results
            liftIO (mapM putStrLn flatResults)
            closeSession
        
fireShrinkRay :: [(String, Int)] -> IO ()
fireShrinkRay [] = helpMsg
fireShrinkRay a = do
    let sessionSettings = parseArgs a defaultArgs
    openSite sessionSettings
    return ()

main :: IO ()
main = do
    userArgs <- getArgs
    fireShrinkRay (zip userArgs [0..])
