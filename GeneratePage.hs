import Control.Arrow          ( second )
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

type SimplePoints = [Double]

type GroupRating = (GroupKey, Double)

data Group = Group { groupKey :: GroupKey, points :: Points }

simplePoints :: Group -> SimplePoints
simplePoints = map ownPoints . points

group :: Int -> String -> Points -> Group
group n code = Group (GroupKey n code)

data Round = Round { name :: String, number :: Int, possible :: Double, groupRatings :: [GroupRating] }

data GroupKey = GroupKey { groupNumber :: Int, code :: String }

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
  

{-group1 :: Group
group1 = group 1 "sdig1o" [4,6,2,9,3,1.5]

group2 :: Group
group2 = group 2 "aikp25" [6,3,2,1,4,7]

group3 :: Group
group3 = group 3 "vzt35d" [1,6,3,9,8,2]

group4 :: Group
group4 = group 4 "fs7g5r" [5.5,4,3,2.5,1,5.5]

group5 :: Group
group5 = group 5 "9hf347" [8,3,1,5,5,6]

group6 :: Group
group6 = group 6 "f853q7" [5,4.5,8,1.5,6,3]

group7 :: Group
group7 = group 7 "pwi5q3" [7,3,7,1,4,8]

group8 :: Group
group8 = group 8 "weu429" [8.5,5,3.5,1,5,4]

group9 :: Group
group9 = group 9 "8fwr7h" [3,7,4,1,8,2]-}

writePointPages :: Labels -> [Group] -> IO ()
writePointPages labels =
  mapM_ (\group -> writeFile (code (groupKey group) ++ ".html")
        (pointPage labels (points group)))

writeGraphPage :: [Group] -> [String] -> IO ()
writeGraphPage groups colors =
  writeFile "index.html"
            (graphPage groups colors)

data Labels = Labels { 
  roundLabel :: String, 
  ownPointsLabel :: String, 
  maxReachedLabel :: String,
  maxReachableLabel :: String
} deriving (Show, Read)

pointPage :: Labels -> Points -> String
pointPage labels points =
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n" ++
  "<html><head><title>PubQuiz: Punktezahl</title>" ++
  "<link rel='stylesheet' type='text/css' href='style.css'/>"++
  "</head><body><div><center>" ++
  mkTable labels points ++
  "</center></div></body></html>"

tableCell :: String -> String
tableCell text = concat ["<td>", text, "</td>"]

tableRow :: String -> String
tableRow text = concat ["<tr>", text, "</tr>\n"]

headerCell :: String -> String
headerCell text = concat ["<th>", text, "</th>"]

mkTableLine :: RoundRating -> String
mkTableLine rating =   
  tableRow (concatMap (tableCell . show) [ownPoints rating, maxReached rating, reachablePoints rating])

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
  "] ," ++ "yAxisID: 'y-axis-1'}"

roundList :: String -> Int -> String
roundList roundName n = concat (zipWith (\r i -> unwords [r, show i]) (repeat roundName) [1 .. n])

graphPage :: [Group] -> [Color] -> String
graphPage groups colors =
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
  \    labels: ['Runde 1', 'Runde 2', 'Runde 3'\
  \            ,'Runde 4', 'Runde 5', 'Runde 6'],\
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
  \     text: 'Chart.js Line Chart - Multi Axis'\
  \ },\
  \ scales: {\
  \ yAxes: [\
  \   { type: 'linear', display: true, position: 'left', id: 'y-axis-1',\
  \     tick: { min: '0', max: '50'} } ]\
  \ }}});};</script></body></html>"

groups = undefined --[group1,group2,group3,group4,group5,group6,group7,group8,group9]

defaultLabels :: Labels
defaultLabels = Labels { 
  roundLabel = "Runde", 
  ownPointsLabel = "Erreichte Punkte", 
  maxReachedLabel = "Erreichte Höchstpunktzahl",
  maxReachableLabel = "Erreichbare Punkte"
}

readLabels :: IO Labels
readLabels = fmap (read :: String -> Labels) (readFile "./labels.txt") `catch` handle where
  handle :: IOException -> IO Labels
  handle _ = putStrLn "labels.txt not found - using default labels." >> return defaultLabels

main :: IO ()
main = do
  labels <- readLabels
  writePointPages labels groups
  writeGraphPage groups colors
