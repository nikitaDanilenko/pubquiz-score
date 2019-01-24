import           Data.List (intercalate)

type Points = [Float]
data Group = Group { no :: Int, code :: String, points :: Points }

group1 :: Group
group1 = Group 1 "sdig1o" [4,6,2,9,3,1.5]

group2 :: Group
group2 = Group 2 "aikp25" [6,3,2,1,4,7]

group3 :: Group
group3 = Group 3 "vzt35d" [1,6,3,9,8,2]

group4 :: Group
group4 = Group 4 "fs7g5r" [5.5,4,3,2.5,1,5.5]

group5 :: Group
group5 = Group 5 "9hf347" [8,3,1,5,5,6]

group6 :: Group
group6 = Group 6 "f853q7" [5,4.5,8,1.5,6,3]

group7 :: Group
group7 = Group 7 "pwi5q3" [7,3,7,1,4,8]

group8 :: Group
group8 = Group 8 "weu429" [8.5,5,3.5,1,5,4]

group9 :: Group
group9 = Group 9 "8fwr7h" [3,7,4,1,8,2]

writePointPages :: [Group] -> IO ()
writePointPages xs =
  mapM_ (\group -> writeFile (code group ++ ".html")
        (pointPage (points group)))
        xs

writeGraphPage :: [Group] -> [String] -> IO ()
writeGraphPage groups colors =
  writeFile "index.html"
            (graphPage groups colors)

pointPage :: Points -> String
pointPage points =
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n" ++
  "<html><head><title>PubQuiz: Punktezahl</title>" ++
  "<link rel='stylesheet' type='text/css' href='style.css'/>"++
  "</head><body><div><center><h1>" ++
  show (sum points) ++
  "</h1></center></div></body></html>"

colors = [ "rgb(255, 99, 132)"
         , "rgb(255, 159, 64)"
         , "rgb(255, 205, 86)"
         , "rgb(75, 192, 192)"
         , "rgb(54, 162, 235)"
         , "rgb(153, 102, 255)"
         , "rgb(201, 203, 207)"
         , "rgb(44, 34, 73)"
         , "rgb(142, 64, 255)"]

rounds = map (("Runde " ++) . show) [(1::Int)..]

toDataset g c =
  "{" ++ "label: '" ++ show (no g) ++ "'" ++
  "," ++ "borderColor: " ++ show c ++
  "," ++ "backgroundColor: " ++ show c ++
  "," ++ "fill: " ++ "false" ++
  "," ++ "data: [" ++ intercalate ","
                            (zipWith (\x y -> "{ x: '" ++ x ++ "' , y: '" ++ show y ++ "'}")
                                     rounds
                                     (tail (scanl (+) 0 (points g)))) ++
  "] ," ++ "yAxisID: 'y-axis-1'}"

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

groups = [group1,group2,group3,group4,group5,group6,group7,group8,group9]

main :: IO ()
main = do
  writePointPages groups
  writeGraphPage groups colors
