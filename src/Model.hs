module Model where

import Char
--import Time.ClockTime


data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum)

data Train = Tr String 
instance Show Train where
    show (Tr name) = "Pociag [" ++ name ++ "]\n"
instance Nameable Train where
    getName (Tr name) = name

--konstruktor: St name [lista pociagow]
data Station = St String [Arrival]
instance Show Station where
    show (St name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)
instance Nameable Station where
    getName (St name arrs) = name

insertArrival :: Station -> Arrival -> Station
insertArrival (St name arrs) arr = St name (arr:arrs)

getArrivals :: Station -> [Arrival]
getArrivals (St name arrs) = arrs


--konstruktor: Arr pociag czasPrzyjazdu dokadDalej
data Arrival = Arr Train String Station
instance Show Arrival where
    show (Arr tr time st) = time ++ " " ++ getName tr ++ " " ++ getName st ++ "\n"

class StationDatabases d where
    empty      :: d
    getObjects :: d -> [Station]
    setObjects :: [Station] -> d -> d
    insertStation     :: [Station] -> d -> d
    insertStation (st) db = setObjects os' db where
                  os' = st ++ os
                  os =  getObjects db

    findByName :: String -> d -> Station
    modifyStation :: (Station -> a -> Station) -> String -> a -> d -> d
    modifyStation f name item db = setObjects os' db where
                         os' = map (\x -> if (getName x) == name then f x item  else x) arr
                         arr = getObjects db

class Nameable d where
    getName :: d -> String

data StationDB = SDB [Station]
instance Show StationDB where
    show (SDB arr) = concat (map show arr)

instance StationDatabases StationDB where
    empty = SDB []
    getObjects (SDB st) = st
    setObjects x (SDB st) = SDB x
    findByName name db = head (filter (\(St n arr) -> n == name) (getObjects db))

haqify s = "Haq! " ++ s