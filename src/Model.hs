
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

data (Named a) => DB a = SDB [a] | TDB [a]

data DBS = DBS (DB Station) (DB Train)

data When = When [Day] TimeOfDay


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

-----------------------------------------
--class instance definitions-------------
-----------------------------------------
instance (Named a,Show a) => Show (DB a) where
    show (SDB st) = concat (map show st)
    show (TDB tr) = concat (map show tr)

instance Named Train where
    getName (Train name days) = name

instance Named Arrival where
    getName (Arrival tr time st) = getName tr

instance Named Station where
    getName (Station name arrs) = name

instance Database DB where
    --empty = SDB []
    getObjects (SDB st) = st
    setObjects x (SDB st) = SDB x
    --getObjects (TDB tr) = tr
    --setObjects x (TDB tr) = TDB x

instance Show Train where
    show (Train name days) = "Pociag [" ++ name ++ "]\n"

instance Show Station where
    show (Station name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)

instance Show Arrival where
    show (Arrival tr time st) = show time ++ " " ++ getName tr ++ " " ++ getName st ++ "\n"

----------------------------------------
--Methods-------------------------------
----------------------------------------
modifyStation :: (Station -> [a] -> Station) -> String -> [a] -> DB Station -> DB Station
modifyStation f name items db = setObjects os' db where
    os' = map (\x -> if (getName x) == name then f x items  else x) arr
    arr = getObjects db

insertArrivals :: Station -> [Arrival] -> Station
insertArrivals (Station name arrs) arr = Station name (arr ++ arrs)

getArrivals :: Station -> [Arrival]
getArrivals (Station name arrs) = arrs

addStation :: String-> DB Station  -> DB Station
addStation name db = insert [(Station name [])] db

eraseStation :: String -> DBS -> DBS
--TO DO
eraseStation name (DBS sdb tdb) = (DBS sdb tdb)

addTrain :: String -> [Day] -> DBS -> DBS
addTrain name days (DBS sdb tdb) = DBS sdb (insert [(Train name days)] tdb)

addStationToTrain :: String -> String ->String -> TimeOfDay -> TimeOfDay -> DBS -> DBS
addStationToTrain stName stNext trName inTime outTime (DBS sdb tdb) = (DBS sdb' tdb) where
    sdb' = modifyStation insertArrivals stName [arrival] sdb
    arrival = (Arrival train outTime nextStation)
    train = head (findAllByName trName tdb)
    nextStation = head (findAllByName stNext sdb)

eraseTrain :: String -> DBS -> DBS
eraseTrain name (DBS sdb tdb) = (DBS (setObjects sdb' sdb) (setObjects tdb' tdb)) where
    sdb' = map (\station -> removeTrainArrival station (getName train)) (getObjects sdb)
    train = head (findAllByName name sdb)
    tdb' = concat (map (\tr -> if getName tr == getName train then [] else [tr]) (getObjects tdb))

removeTrainArrival :: Station -> String -> Station
removeTrainArrival (Station name arrs) trainName = (Station name arrs') where
    arrs' = concat (map (\it -> if trainName == getName it then [] else [it]) arrs)

modifyTrainDays :: String -> [Day] -> DBS -> DBS
modifyTrainDays name days (DBS sdb tdb) = (DBS sdb (setObjects tdb' tdb)) where
    tdb' = concat (map (\train -> if getName train == name then [(Train name days)] else [train]) (getObjects tdb))

getTimetableForStation :: String -> Day -> String
getTimetableForStation name day = "nic na razie" 