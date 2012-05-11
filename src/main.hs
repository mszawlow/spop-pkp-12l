import Model as M
import System.Environment

main :: IO ()

t,m :: StationDB


l = [(Station "Krakow" []),(Station "Warszawa" [])]
t = insertStations l empty
m = modifyStation insertArrivals "Warszawa" [(Arrival (Train "mieszko") "12:34" (findByName "Warszawa" t)),(Arrival (Train "rejtan") "18:22" (findByName "Warszawa" t))] t


main = print m
