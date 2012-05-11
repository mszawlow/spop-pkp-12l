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
    show (Train name days stations) = ret where
                    ret = "Pociag [" ++ name ++ "]\n" ++ join (names stations) " -> "  ++"\n"
                    names a = map (\it -> getName it) a

instance Show Station where
    show (Station name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)

instance Show Arrival where
    show (Arrival tr timeIn timeOut) = show timeIn ++ " " ++ show timeOut ++ " " ++ getName tr ++ "\n"

instance Eq Day


----------------------------------------
--Helper Methods------------------------
----------------------------------------
join :: [String] -> String -> String
join [] separator = ""
join (x:[]) separator = x
join (x:xs) separator = concat ( [x] ++ [separator] ++ [join xs separator])


empty :: DBS
empty = DBS (DB []) (DB [])

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



removeStationFromTrain :: String -> Train -> Train
removeStationFromTrain stId (Train name days st) = (Train name days stations) where
    stations = concat (map (\it -> if getName it == name then [] else [it]) st)



getId :: (Named a) => a -> Id
getId a = (Id (getName a))


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
