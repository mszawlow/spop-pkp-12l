module Model where

import Char
import Data.Time.LocalTime


---------------------------------------
--data definitions---------------------
---------------------------------------
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum)

data Train = Train String

--konstruktor: St name [lista pociagow]
data Station = Station String [Arrival]

--konstruktor: Arr pociag czasPrzyjazdu dokadDalej
data Arrival = Arrival Train String Station

data StationDB = SDB [Station]

data When = When [Day] TimeOfDay


----------------------------------------
--type classes definitions--------------
----------------------------------------
class Named d where
    getName :: d -> String

class StationDatabases d where
    empty      :: d
    getObjects :: d -> [Station]
    setObjects :: [Station] -> d -> d
    insertStations     :: [Station] -> d -> d
    insertStations (st) db = setObjects os' db where
                  os' = st ++ os
                  os =  getObjects db

    findByName :: String -> d -> Station
    modifyStation :: (Station -> [a] -> Station) -> String -> [a] -> d -> d
    modifyStation f name items db = setObjects os' db where
                         os' = map (\x -> if (getName x) == name then f x items  else x) arr
                         arr = getObjects db


-----------------------------------------
--class instance definitions-------------
-----------------------------------------
instance Show StationDB where
    show (SDB arr) = concat (map show arr)

instance StationDatabases StationDB where
    empty = SDB []
    getObjects (SDB st) = st
    setObjects x (SDB st) = SDB x
    findByName name db = head (filter (\(Station n arr) -> n == name) (getObjects db))

instance Named Train where
    getName (Train name) = name

instance Named Station where
    getName (Station name arrs) = name

instance Show Train where
    show (Train name) = "Pociag [" ++ name ++ "]\n"

instance Show Station where
    show (Station name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)

instance Show Arrival where
    show (Arrival tr time st) = time ++ " " ++ getName tr ++ " " ++ getName st ++ "\n"

----------------------------------------
--Methods-------------------------------
----------------------------------------
insertArrivals :: Station -> [Arrival] -> Station
insertArrivals (Station name arrs) arr = Station name (arr ++ arrs)

getArrivals :: Station -> [Arrival]
getArrivals (Station name arrs) = arrs



