module Model where

import Char
--import Time.ClockTime


data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum)

data Train = String

--konstruktor: St name [lista pociagow]
data Station = St String [Arrival]

insertArrival :: Station -> Arrival -> Station
insertArrival (St name arrs) arr = St name (arr:arrs)

--konstruktor: Arr pociag czasPrzyjazdu dokadDalej
data Arrival = Arr Train String Station

class StationDatabases d where

    empty      :: d

    getObjects :: d -> [Station]

    setObjects :: [Station] -> d -> d

    insertStation     :: Station -> d -> d
    insertStation st db = setObjects os' db where
                  os' = st : os
                  os =  getObjects db

    --TO DO
    selectBy   :: (Station -> Bool) -> d -> [Station]
    selectBy f db = filter f (getObjects db)

data StationDB = SDB [Station]

instance StationDatabases StationDB where
    empty = SDB []
    getObjects (SDB st) = st
    setObjects x (SDB st) = SDB x

haqify s = "Haq! " ++ s