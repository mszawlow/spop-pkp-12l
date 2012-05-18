import System.IO
import System.Directory
import Control.Monad
import Model
import API
import Data.Time hiding (Day)

s2d :: String -> TimeOfDay
s2d s = read s::TimeOfDay

getTestData :: DBS
getTestData = ret where
	tdb =  DB [(Train "Mieszko" [Mon, Tue, Fri, Sat, Sun] []), (Train "Chrobry" [Mon, Tue, Fri, Sat, Sun] [])]
	tdb2 = addTrain "Walesa" [Mon, Tue, Fri, Sat] tdb
	tdb3 = addTrain "Pomorzanin" [Mon, Tue] tdb2
	tdb4 = addTrain "Baltyk" [Mon, Tue, Fri] tdb3
	tdb5 = addTrain "Kujawiak" [Mon, Fri] tdb4
	sdb = DB [(Station "Warszawa" []),(Station "Krakow" []),(Station "Zakopane" [])]
	sdb2 = addStation "Wroclaw" sdb
	sdb3 = addStation "Bydgoszcz" sdb2
	sdb4 = addStation "Gdansk" sdb3
	sdb5 = addStation "Szczecin" sdb4
	dbs1 = addStationToTrain "Warszawa" "Mieszko" (s2d "11:10:00") (s2d "11:15:00") (DBS sdb5 tdb5)
	dbs2 = addStationToTrain "Krakow" "Mieszko" (s2d "14:40:00") (s2d "14:45:00") dbs1
	dbs3 = addStationToTrain "Zakopane" "Mieszko" (s2d "16:40:00") (s2d "16:45:00") dbs2
	dbs4 = addStationToTrain "Wroclaw" "Chrobry" (s2d "10:20:00") (s2d "10:22:00") dbs3
	dbs5 = addStationToTrain "Krakow" "Chrobry" (s2d "14:42:00") (s2d "14:47:00") dbs4
	dbs6 = addStationToTrain "Bydgoszcz" "Chrobry" (s2d "16:40:00") (s2d "16:45:00") dbs5
	dbs7 = addStationToTrain "Wroclaw" "Walesa" (s2d "09:00:00") (s2d "09:04:00") dbs6
	dbs8 = addStationToTrain "Warszawa" "Walesa" (s2d "11:00:00") (s2d "11:04:00") dbs7
	dbs9 = addStationToTrain "Warszawa" "Pomorzanin" (s2d "13:00:00") (s2d "13:04:00") dbs8
	dbs10 = addStationToTrain "Gdansk" "Pomorzanin" (s2d "16:00:00") (s2d "16:04:00") dbs9
	dbs11 = addStationToTrain "Gdansk" "Baltyk" (s2d "16:10:00") (s2d "16:15:00") dbs10
	dbs12 = addStationToTrain "Szczecin" "Baltyk" (s2d "18:00:00") (s2d "18:04:00") dbs11
	dbs13 = addStationToTrain "Warszawa" "Kujawiak" (s2d "12:10:00") (s2d "12:15:00") dbs12
	dbs14 = addStationToTrain "Szczecin" "Kujawiak" (s2d "17:00:00") (s2d "17:04:00") dbs13
	      
	ret = dbs14


saveFile = writeFile "dbs.db" (show getTestData)

readFromFile = do
				exists <- doesFileExist "dbs.db"
				if exists	
					then do
						x <- readFile "dbs.db"
						putStrLn (search "Warszawa" "Zakopane" 0 Mon (read "10:00:00"::TimeOfDay) (read x::DBS))
					else do
						writeFile "dbs.db" (show empty)
	
	
