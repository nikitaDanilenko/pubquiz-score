import Control.Arrow          ( second, (&&&), (***), (>>>) )
import Control.Exception      ( catch )
import Control.Exception.Base ( IOException )
import Data.Function          ( on )
import Data.List              ( intercalate, maximumBy )
import Data.Map               ( Map, fromList, unionsWith, toList )
import Data.Ord               ( comparing )

data RoundRating = RoundRating { 
  roundNumber :: Int, 
  maxReached :: Double, 
  reachablePoints :: Double, 
  ownPoints :: Double
}

type Points = [RoundRating]

score :: Points -> (Double, Double)
score points = 
  foldr (\rating (o, r) -> ((ownPoints &&& reachablePoints) >>> ((+) *** (+)) >>> (($ o) *** ($ r))) rating) (0, 0) points

mkSum :: Points -> String
mkSum = score >>> uncurry (\own reachable -> concat [prettyDouble own, "/", prettyDouble reachable])

prettyDouble :: Double -> String
prettyDouble d = short where
  int = round d :: Int
  short | (fromIntegral int :: Double) == d = show int
        | otherwise = show d

type SimplePoints = [Double]

type GroupRating = (GroupKey, Double)

data Group = Group { groupKey :: GroupKey, points :: Points }

simplePoints :: Group -> SimplePoints
simplePoints = map ownPoints . points

data Round = Round { name :: String, number :: Int, possible :: Double, groupRatings :: [GroupRating] }

fromIndex :: [Code] -> String -> Int -> Double -> [Double] -> Round
fromIndex groupCodes name number possible points = Round name number possible ratings where
  ratings = zipWith3 (\i c p -> (GroupKey i c, p)) [1 .. ] groupCodes points

type Code = String

data GroupKey = GroupKey { groupNumber :: Int, code :: Code }

instance Eq GroupKey where
  (==) = (==) `on` groupNumber

instance Ord GroupKey where
  compare = compare `on` groupNumber

-- Computes the maximum number reached in a given round.
maxInRound :: Round -> Double
maxInRound = snd . maximumBy (comparing snd) . groupRatings

roundRating :: Round -> Map GroupKey Points
roundRating round = fromList ratings where
  n = number round
  reachable = possible round
  maxReached = maxInRound round
  gs = groupRatings round
  ratings = map (second (pure . RoundRating n maxReached reachable)) gs
 
mkGroups :: [Round] -> [Group]
mkGroups = map (uncurry Group) . toList . unionsWith (++) . map roundRating

writePointPages :: Labels -> [Group] -> IO ()
writePointPages labels =
  mapM_ (\group -> writeFile (code (groupKey group) ++ ".html")
        (pointPage labels (points group)))

writeGraphPage :: Labels -> Int -> [Group] -> [String] -> IO ()
writeGraphPage labels rounds groups colors =
  writeFile "index.html"
            (graphPage labels rounds groups colors)

data Labels = Labels { 
  roundLabel :: String, 
  ownPointsLabel :: String, 
  maxReachedLabel :: String,
  maxReachableLabel :: String,
  backToChartView :: String,
  mainLabel :: String
} deriving (Show, Read)

htmlSafeChar :: Char -> String
htmlSafeChar 'ö' = "&ouml;"
htmlSafeChar 'ä' = "&auml;"
htmlSafeChar 'ü' = "&uuml;"
htmlSafeChar 'ß' = "&szlig;"
htmlSafeChar c = [c]

htmlSafeString :: String -> String
htmlSafeString = concatMap htmlSafeChar

centerDiv :: String -> String
centerDiv text = concat ["<div><center>", text, "</center></div>"]

pointPage :: Labels -> Points -> String
pointPage labels points =
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n" ++
  "<html><head><title>PubQuiz: Punktezahl</title>" ++
  "<link rel='stylesheet' type='text/css' href='style.css'/>"++
  "</head><body>"++
  centerDiv (h1 (mkSum points)) ++
  centerDiv (mkTable labels points) ++
  centerDiv (mkButton (backToChartView labels)) ++
  "</body></html>"

h1 :: String -> String
h1 text = concat ["<h1>", text, "</h1>"]

tableCell :: String -> String
tableCell text = concat ["<td>", text, "</td>"]

tableRow :: String -> String
tableRow text = concat ["<tr>", text, "</tr>\n"]

headerCell :: String -> String
headerCell text = concat ["<th>", text, "</th>"]

mkTableLine :: RoundRating -> String
mkTableLine rating =   
  tableRow (concatMap tableCell [
    show (roundNumber rating), 
    prettyDouble (ownPoints rating), 
    prettyDouble (maxReached rating), 
    prettyDouble (reachablePoints rating)
    ])

tableHeader :: Labels -> String
tableHeader labels = 
  tableRow (concatMap headerCell [
    roundLabel labels, 
    ownPointsLabel labels, 
    maxReachedLabel labels, 
    maxReachableLabel labels
    ]
  )

mkTable :: Labels -> Points -> String
mkTable labels points = 
  concat [
  "<table>\n", 
  tableHeader labels, 
  concatMap mkTableLine points,
  "</table>"]

mkButton :: String -> String
mkButton text = concat ["<a href=\"./index.html\" class=\"button\">", text, "</a>"]

type Color = String

mkColor :: Int -> Int -> Int -> Color
mkColor red green blue = "rgb(" ++ intercalate "," (map show [red, green, blue]) ++ ")"

colors :: [Color]
colors = cycle [ "rgb(255, 99, 132)"
         , "rgb(255, 159, 64)"
         , "rgb(255, 205, 86)"
         , "rgb(75, 192, 192)"
         , "rgb(54, 162, 235)"
         , "rgb(153, 102, 255)"
         , "rgb(201, 203, 207)"
         , "rgb(44, 34, 73)"
         , "rgb(142, 64, 255)"]

rounds :: [String]
rounds = map (("Runde " ++) . show) [(1::Int)..]

toDataset :: Group -> Color -> String
toDataset g c =
  "{" ++ "label: '" ++ show (groupNumber (groupKey g)) ++ "'" ++
  "," ++ "borderColor: " ++ show c ++
  "," ++ "backgroundColor: " ++ show c ++
  "," ++ "fill: " ++ "false" ++
  "," ++ "data: [" ++ intercalate ","
                            (zipWith (\x y -> "{ x: '" ++ x ++ "' , y: '" ++ show y ++ "'}")
                                     rounds
                                     (tail (scanl (+) 0 (simplePoints g)))) ++
  "], " ++ 
  "lineTension: 0, " ++
  "yAxisID: 'y-axis-1'}"

roundList :: String -> Int -> String
roundList roundName n = 
  intercalate "," (zipWith (\r i -> unwords ["'", r, show i, "'"]) (repeat roundName) [1 .. n])

graphPage :: Labels -> Int -> [Group] -> [Color] -> String
graphPage labels rounds groups colors =
  "<html>\
  \<head><title>Pubquiz</title>\
  \<script src='https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.7.3/Chart.bundle.min.js'></script>\
  \<style>\
  \canvas {\
  \        -moz-user-select: none;\
  \        -webkit-user-select: none;\
  \        -ms-user-select: none;\
  \}</style>\
  \</head>\
  \<body>\
  \<div style='width:75%;'>\
  \<canvas id='canvas'></canvas>\
  \</div>\
  \<script>\
  \var lineChartData = {\
  \    labels: ["
  ++ 
  roundList (roundLabel labels) rounds
  ++ 
  "], \ 
  \    datasets:["
  ++
  intercalate "," (zipWith toDataset groups colors)
  ++
  "]};\
  \window.onload = function() {\
  \  var ctx = document.getElementById('canvas').getContext('2d');\
  \  window.myLine = Chart.Line(ctx, {\
  \   data: lineChartData,\
  \   options: {\
  \   responsive: true,\
  \   hoverMode: 'index',\
  \   stacked: false,\
  \   title: {\
  \     display: true,\
  \     text: '"
  ++ 
  mainLabel labels 
  ++ 
  "\'"
  ++
  " },\
  \ scales: {\
  \ yAxes: [\
  \   { type: 'linear', display: true, position: 'left', id: 'y-axis-1',\
  \     tick: { min: '0', max: '50'} } ]\
  \ }}});};</script>\
  \ <div id = 'copyright'>Powered by <a href='https://www.chartjs.org'>Chart.js</a></div>\
  \</body></html>"

defaultLabels :: Labels
defaultLabels = Labels { 
  roundLabel = htmlSafeString "Runde", 
  ownPointsLabel = htmlSafeString "Erreichte Punkte", 
  maxReachedLabel = htmlSafeString "Erreichte Höchstpunktzahl",
  maxReachableLabel = htmlSafeString "Erreichbare Punkte",
  backToChartView = htmlSafeString "Gesamtansicht",
  mainLabel = htmlSafeString "Pubquiz"
}

readLabels :: IO Labels
readLabels = fmap (read :: String -> Labels) (readFile "./labels.txt") `catch` handle where
  handle :: IOException -> IO Labels
  handle _ = putStrLn "labels.txt not found - using default labels." >> return defaultLabels

readPoints :: String -> (Double, [Double])
readPoints [] = (0, [])
readPoints text = (total, ps) where
  (total : _ : ps) = map read (words text)

parseCodesAndRounds :: String -> String -> ([String], [Round])
parseCodesAndRounds _ [] = ([], [])
parseCodesAndRounds roundName text = (codes, rounds) where
  (l : ls) = lines text
  codes = words l
  points = map readPoints ls
  indexedPoints = zip [1 ..] points
  rounds = map (\(i, (total, ps)) -> fromIndex codes roundName i total ps) indexedPoints

readCodesAndRounds :: String -> IO ([String], [Round])
readCodesAndRounds roundLabel =
  fmap (parseCodesAndRounds roundLabel) (readFile "rounds.txt") `catch` handle where
    handle :: IOException -> IO ([String], [Round]) 
    handle _ = putStrLn "Unexpected format or missing file. No output generated." >> return ([], [])

main :: IO ()
main = do
  labels <- readLabels
  (codes, rounds) <- readCodesAndRounds (roundLabel labels)
  let groups = mkGroups rounds
      n = length rounds
  writePointPages labels groups
  writeGraphPage labels n groups colors