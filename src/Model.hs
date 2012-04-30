{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Model where

import Char
import Data.Time hiding (Day)

---------------------------------------
--data definitions---------------------
---------------------------------------
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum)

data Train = Train String [Day]

--konstruktor: St name [lista pociagow]
data Station = Station String [Arrival]

--konstruktor: Arr pociag czasPrzyjazdu dokadDalej
data Arrival = Arrival Train TimeOfDay Station

data StationDB = SDB [Station]

data TrainDB = TDB [Train]

data DBS = DBS StationDB TrainDB

data When = When [Day] TimeOfDay

asd :: a1 -> a
asd (SDB a) = a
asd (TDB a) = a

----------------------------------------
--type classes definitions--------------
----------------------------------------
class Named d  where
    getName :: d -> String

class Database d a where
    empty      :: d
    getObjects :: d -> a
    setObjects :: [a] -> d -> d
    insert     :: [a] -> d -> a
    insert (st) db = setObjects os' db where
                 os' = st ++ os
                 os =  getObjects db

    findAllByName :: String -> d -> [a]
    exists :: String -> d -> Bool
    exists name db = (findAllByName name db) /= []

-----------------------------------------
--class instance definitions-------------
-----------------------------------------
instance Show StationDB where
    show (SDB arr) = concat (map show arr)


instance Named Train where
    getName (Train name days) = name

instance Named Arrival where
    getName (Arrival tr time st) = getName tr

instance Named Station where
    getName (Station name arrs) = name

instance Database StationDB Station where
    empty = SDB []
    getObjects (SDB st) = st
    setObjects x (SDB st) = SDB x
    findAllByName name db = filter (\(Station n arr) -> n == name) (getObjects db)

instance Database TrainDB Train where
    empty = TDB []
    getObjects (TDB tr) = tr
    setObjects x (TDB tr) = TDB x
    findAllByName name db = filter (\(Train n days) -> n == name) (getObjects db)

instance Show Train where
    show (Train name) = "Pociag [" ++ name ++ "]\n"

instance Show Station where
    show (Station name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)

instance Show Arrival where
    show (Arrival tr time st) = time ++ " " ++ getName tr ++ " " ++ getName st ++ "\n"

----------------------------------------
--Methods-------------------------------
----------------------------------------
modifyStation :: (Station -> [a] -> Station) -> String -> [a] -> StationDB -> StationDB
modifyStation f name items db = setObjects os' db where
    os' = map (\x -> if (getName x) == name then f x items  else x) arr
    arr = getObjects db

insertArrivals :: Station -> [Arrival] -> Station
insertArrivals (Station name arrs) arr = Station name (arr ++ arrs)

getArrivals :: Station -> [Arrival]
getArrivals (Station name arrs) = arrs

addStation :: String-> StationDB  -> StationDB
addStation name db = insert [(Station name [])] db

eraseStation :: String -> DBS -> DBS
--TO DO
eraseStation name (DBS sdb tdb) = (DBS sdb tdb)

addTrain :: String -> [Day] -> DBS -> DBS
addTrain name days (DBS sdb tdb) = DBS sdb (insert [(Train name days)] tdb)

addStationToTrain :: String -> String ->String -> TimeOfDay -> TimeOfDay -> DBS -> DBS
addStationToTrain stName stNext trName inTime outTime (DBS sdb tdb) = (DBS sdb' tdb) where
    sdb' = modifyStation insertArrivals (head findAllByName stName sdb) [(Arrival (head findAllByName trName tdb) outTime (head findAllByName stNext sdb))] sdb

eraseTrain :: String -> DBS -> DBS
eraseTrain name (DBS sdb tdb) = (DBS (setObjects sdb' sdb) (setObjects tdb' tdb)) where
    sdb' = map (\station -> removeTrainArrival station (getName train)) (getObjects sdb)
    train = head (findAllByName name sdb)
    tdb' = concat (map (\tr -> if getName tr == getName train then [] else [tr]) getObjects tdb )

removeTrainArrival :: Station -> String -> Station
removeTrainArrival (Station name arrs) trainName = Station (name arrs') where
    arrs' = concat (map (\it -> if trainName == getName it then [] else [it]) arrs)

modifyTrainDays :: String -> [Day] -> DBS -> DBS
modifyTrainDays name days (DBS sdb tdb) = (DBS sdb (setObjects tdb' tdb)) where
    tdb' = concat (map (\train -> if getName train == name then [(Train name days)] else [train]) getObjects tdb)

getTimetableForStation :: String -> Day -> String
getTimetableForStation name day = "nic na razie"