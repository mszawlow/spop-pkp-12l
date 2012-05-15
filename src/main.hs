import API
import Model
import System.Environment
import Data.Time hiding (Day)
import Menu

main :: IO ()

s2d :: String -> TimeOfDay
s2d s = read s::TimeOfDay





getTestData :: DBS
getTestData = ret where
    tdb =  DB [(Train "Mieszko" [Mon, Tue, Fri, Sat, Sun] []), (Train "Chrobry" [Mon, Tue, Fri, Sat, Sun] [])]
    tdb2 = addTrain "Walesa" [Mon,Tue,Fri,Sat] tdb
    sdb = DB [(Station "Warszawa" []),(Station "Krakow" []),(Station "Zakopane" [])]
    sdb2 = addStation "Wroclaw" sdb
    sdb3 = addStation "Bydgoszcz" sdb2
    dbs1 = addStationToTrain "Warszawa" "Mieszko" (s2d "11:10:00") (s2d "11:15:00") (DBS sdb3 tdb2)
    dbs2 = addStationToTrain "Krakow" "Mieszko" (s2d "14:40:00") (s2d "14:45:00") dbs1
    dbs3 = addStationToTrain "Zakopane" "Mieszko" (s2d "16:40:00") (s2d "16:45:00") dbs2
    dbs4 = addStationToTrain "Wroclaw" "Chrobry" (s2d "10:20:00") (s2d "10:22:00") dbs3
    dbs5 = addStationToTrain "Krakow" "Chrobry" (s2d "14:42:00") (s2d "14:47:00") dbs4
    dbs6 = addStationToTrain "Bydgoszcz" "Chrobry" (s2d "16:40:00") (s2d "16:45:00") dbs5
    dbs7 = addStationToTrain "Wroclaw" "Walesa" (s2d "09:00:00") (s2d "09:04:00") dbs6
    dbs8 = addStationToTrain "Warszawa" "Walesa" (s2d "11:00:00") (s2d "11:04:00") dbs7
        
    ret = dbs8

main = putStrLn "asd" --searchConn "Warszawa" "Zakopane" 0 [] getTestData



{- menu getTestData{-ret where
    x (DBS (DB a) _) = map show a
    y (DBS _ (DB a)) = map show a
    tdb (DBS _ a) = a
    ret = do putStrLn (concat (x getTestData) ++ concat (y getTestData) ++ getTimetableForStation "Warszawa" Mon getTestData)-} -}