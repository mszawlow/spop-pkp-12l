
module Model where

import Char
import Data.Time hiding (Day)

---------------------------------------
--data definitions---------------------
---------------------------------------
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum, Read)

-- nazwa dni stacje
data Train = Train String [Day] [Id]

--konstruktor: St name [lista pociagow]
data Station = Station String [Arrival]

--konstruktor: Arr pociagId czasPrzyjazdu
data Arrival = Arrival Id TimeOfDay TimeOfDay

data (Named a) => DB a = DB [a]

data DBS = DBS (DB Station) (DB Train)

data When = When [Day] TimeOfDay

data Id = Id String


----------------------------------------
--type classes definitions--------------
----------------------------------------
class Named d  where
    getName :: d -> String


class Database d where
    --empty      :: d
    getObjects :: (Named a) => (d a) -> [a]
    setObjects :: (Named a) => [a] -> (d a) -> (d a)
    insert     :: (Named a) =>[a] -> (d a) -> (d a)
    insert st db = setObjects os' db where
                 os' = st ++ os
                 os =  getObjects db

    findAllByName :: (Named a) => String -> (d a) -> [a]
    findAllByName name db = filter (\it -> getName it == name) (getObjects db)

    exists :: (Named a,Eq a) => String -> (d a) -> Bool
    exists name db = (findAllByName name db) /= []

    remove :: (Named a) => String -> (d a) -> (d a)
    remove name db = setObjects newObj db where
                 obj = getObjects db
                 newObj = concat (map (\it -> if getName it == name then [] else [it]) obj)
-----------------------------------------
--class instance definitions-------------
-----------------------------------------
instance (Named a,Show a) => Show (DB a) where
    show (DB x) = concat (map show x)

instance Named Id where
    getName (Id a) = a

instance Named Train where
    getName (Train name days stations) = name

instance Named Arrival where
    getName (Arrival tr time st) = getName tr

instance Named Station where
    getName (Station name arrs) = name

instance Database DB where
    getObjects (DB x) = x
    setObjects a (DB x) = DB a

instance Show Train where
    show (Train name days stations) = "Pociag [" ++ name ++ "]\n"

instance Show Station where
    show (Station name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)

instance Show Arrival where
    show (Arrival tr timeIn timeOut) = show timeIn ++ " " ++ show timeOut ++ " " ++ getName tr ++ "\n"

instance Eq Day

---------------------------------------
--API----------------------------------
---------------------------------------


----------------------------------------
--Helper Methods------------------------
----------------------------------------
empty :: DBS
empty = DBS (DB []) (DB [])

modifyStation :: (Station -> [a] -> Station) -> String -> [a] -> DB Station -> DB Station
modifyStation f name items db = setObjects os' db where
    os' = map (\x -> if (getName x) == name then f x items  else x) arr
    arr = getObjects db

insertArrivals :: Station -> [Arrival] -> Station
insertArrivals (Station name arrs) arr = Station name (arr ++ arrs)

replaceArrivals :: Station -> [Arrival] -> Station
replaceArrivals (Station name arrs) arr = Station name arr

renameStation :: Station -> [String] -> Station
renameStation (Station _ arrs) names = (Station (head names) arrs)

getArrivals :: Station -> [Arrival]
getArrivals (Station name arrs) = arrs

getDays :: Train -> [Day]
getDays (Train _ days _) = days

addStation :: String-> DB Station  -> DB Station
addStation name db = insert [(Station name [])] db

eraseStation :: String -> DBS -> DBS
eraseStation name (DBS sdb tdb) = (DBS sdb' tdb') where
    sdb' = remove name sdb
    trains = getObjects tdb
    newTrains = map (removeStationFromTrain name) trains
    tdb' = setObjects newTrains tdb

removeStationFromTrain :: String -> Train -> Train
removeStationFromTrain stId (Train name days st) = (Train name days stations) where
    stations = concat (map (\it -> if getName it == name then [] else [it]) st)


addTrain :: String -> [Day] -> DB Train -> DB Train
addTrain name days db = insert [(Train name days [])] db
getId :: (Named a) => a -> Id
getId a = (Id (getName a))

renameTrain :: String -> String -> DBS -> DBS
renameTrain old new (DBS sdb tdb) = (DBS sdb tdb') where
    tdb' = setObjects trains' tdb
    trains = getObjects tdb
    trains' = map (\(Train id st) -> if (old == id) then (Train new st) else (Train id st)) trains


modifyStationToTrain :: String -> String -> TimeOfDay -> TimeOfDay -> DBS -> DBS
modifyStationToTrain stName trName inTime outTime (DBS sdb tdb) = (DBS sdb' tdb) where
    (Station name arrs) = head (findAllByName stName sdb)
    newArrs = map (\(Arrival trainId x y) -> if (trainId == trName) then (Arrival trainId inTime outTime) else (Arrival trainId x y)) arrs
    sdb' = modifyStation replaceArrivals stName newArrs sdb

addStationToTrain :: String -> String -> TimeOfDay -> TimeOfDay -> DBS -> DBS
addStationToTrain stName trName inTime outTime (DBS sdb tdb) = (DBS sdb' tdb') where
    sdb' = modifyStation insertArrivals stName [arrival] sdb
    arrival = (Arrival (getId train) inTime outTime)
    train = head (findAllByName trName tdb)
    tdb' = setObjects newTrains tdb
    newTrains = concat (
                   map
                   (\(Train n d s) ->
                        if n == trName
                        then [(Train n d (s ++ [(Id stName)]))]
                        else [(Train n d s)])
                   (getObjects tdb)
                  )

eraseTrain :: String -> DBS -> DBS
eraseTrain name (DBS sdb tdb) = (DBS (setObjects sdb' sdb) tdb') where
    sdb' = map (\station -> removeTrainArrival station (getName train)) (getObjects sdb)
    train = head (findAllByName name sdb)
    tdb' = remove name tdb

--API
eraseStationFromTrain :: String -> String -> DBS -> DBS
eraseStationFromTrain stName trName (DBS sdb tdb) = (DBS (setObjects sdb' sdb) (setObjects tdb' tdb)) where
    sdb' = map (\station -> removeTrainArrival station trName) (getObjects sdb)
    tdb' = map (\tr -> if ((getName tr) == trName) then removeStationFromTrain stName tr  else tr) (getObjects tdb)


removeTrainArrival :: Station -> String -> Station
removeTrainArrival (Station name arrs) trainName = (Station name arrs') where
    arrs' = concat (
                    map
                    (\it ->
                         if trainName == getName it
                         then []
                         else [it])
                    arrs
                   )

modifyTrainDays :: String -> [Day] -> DBS -> DBS
modifyTrainDays name days (DBS sdb tdb) = (DBS sdb (setObjects tdb' tdb)) where
    tdb' = concat (
                   map
                   (\(Train n d s) ->
                        if n == name
                        then [(Train n days s)]
                        else [(Train n d s)])
                   (getObjects tdb)
                  )

getTimetableForStation :: String -> Day -> DBS -> String
getTimetableForStation name day (DBS sdb tdb)  = ret where
    station = head (findAllByName name sdb)
    arrivals = getArrivals station
    ret = concat (
                  map (\arr ->
                       if isTrainOnTimetable (getName arr) day tdb
                       then show arr
                       else []
                      )
                  arrivals)


isTrainOnTimetable :: String -> Day -> DB Train -> Bool
isTrainOnTimetable name day tdb = ret where
    train = head (findAllByName name tdb)
    ret = elem day (getDays train)

isStationInTrain :: String -> String -> DB Train -> Bool
isStationInTrain stName trName tdb = ret where
    (Train name stations) = head (findAllByName trName tdb)
    ret = elem stName stations