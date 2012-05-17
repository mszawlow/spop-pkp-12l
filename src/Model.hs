module Model where

import Char
import Data.Time hiding (Day)

---------------------------------------
--data definitions---------------------
---------------------------------------
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum, Read)

-- nazwa dni stacje
data Train = Train String [Day] [Id] deriving (Show)

--konstruktor: St name [lista pociagow]
data Station = Station String [Arrival] deriving (Show)

--konstruktor: Arr pociagId czasPrzyjazdu czasOdjazdu
data Arrival = Arrival Id Id TimeOfDay TimeOfDay deriving (Show)

data (Named a) => DB a = DB [a] deriving (Show)

data DBS = DBS (DB Station) (DB Train) deriving (Show)

data Id = Id String deriving (Show)


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
    
    findAllByNames :: (Named a) => [String] -> (d a) -> [a]
    findAllByNames names db = filter (\it -> elem (getName it) names) (getObjects db)


    exists :: (Named a) => String -> (d a) -> Bool
    exists name db = length (findAllByName name db) > 0

    remove :: (Named a) => String -> (d a) -> (d a)
    remove name db = setObjects newObj db where
                 obj = getObjects db
                 newObj = concat (map (\it -> if getName it == name then [] else [it]) obj)
-----------------------------------------
--class instance definitions-------------
-----------------------------------------


instance Named Id where
    getName (Id a) = a

instance Named Train where
    getName (Train name days stations) = name

instance Named Arrival where
    getName (Arrival tr _ time st) = getName tr

instance Named Station where
    getName (Station name arrs) = name

instance Database DB where
    getObjects (DB x) = x
    setObjects a (DB x) = DB a

{-instance Show Train where
    show (Train name days stations) = ret where
                    ret = "Pociag [" ++ name ++ "]\n" ++ join (names stations) " -> "  ++"\n"
                    names a = map (\it -> getName it) a

instance Show Station where
    show (Station name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)

instance Show Arrival where
    show (Arrival tr st timeIn timeOut) = getName st ++ " : " ++ show timeIn ++ " " ++ show timeOut ++ " " ++ getName tr ++ " -> "

instance (Named a,Show a) => Show (DB a) where
    show (DB x) = concat (map show x)
-}

instance Eq Day where
    c == c' = fromEnum c == fromEnum c'


----------------------------------------
--Helper Methods------------------------
----------------------------------------


----------------PRINT-----------------------

printNextArrival :: String -> Arrival -> DBS -> String
printNextArrival stName arr (DBS sdb tdb) = ret where
    (Station _ arrs) = head (findAllByName stName sdb)
    arrStr = map show arrs
    ret = show arr ++ getNext stations stName
    (Train _ _ stations) = head (findAllByName (getName arr) tdb)
    getNext (x:[]) n = "Ostatnia stacja\n"
    getNext (x:xs) n  | (n == getName x) = getName (head xs)
                      | otherwise = getNext xs n

printTrain :: Train -> String
printTrain (Train name days stations) = ret where
                    ret = "Pociag [" ++ name ++ "]\n" ++ join (names stations) " -> "  ++"\n"
                    names a = map (\it -> getName it) a

printStation :: Station -> String
printStation (Station name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)

printArrival :: Arrival -> String
printArrival (Arrival tr st timeIn timeOut) = getName st ++ " : " ++ show timeIn ++ " " ++ show timeOut ++ " " ++ getName tr ++ " -> "

---------------CHECK-----------------------
--Nazwa pociagu -> dzien -> baza pociagow -> true/false
--zwraca czy pociag jedzie danego dnia
isTrainOnTimetable :: String -> Day -> DB Train -> Bool
isTrainOnTimetable name day tdb = ret where
    train = head (findAllByName name tdb)
    ret = elem day (getDays train)

--Nazwa stacji -> nazwa pociagu -> baza pociagow -> true/false
--zwraca odpowiedz na pytanie czy pociag zatrzymuje sie na danej stacji
isStationInTrain :: String -> String -> DB Train -> Bool
isStationInTrain stName trName tdb = ret where
    (Train name days stations) = head (findAllByName trName tdb)
    ret = elem stName (map getName stations)



--bierze nastepna stacje pociagu
{-getNextStation :: String -> String -> DBS -> String
getNextStation stName trName (DBS sdb tdb) = ret where
    ret = getNext stations stName
    (Train _ _ stations) = head (findAllByName trName tdb)
    getNext (x:xs) n  | (n == getName x) = getName (head xs)
                      | otherwise = getNext xs n
-}

join :: [String] -> String -> String
join [] separator = ""
join (x:[]) separator = x
join (x:xs) separator = concat ( [x] ++ [separator] ++ [join xs separator])


empty :: DBS
empty = DBS (DB []) (DB [])

insertArrivals :: Station -> [Arrival] -> Station
insertArrivals (Station name arrs) arr = Station name (insertArrivalInOrder (head arr) arrs)

insertArrivalInOrder :: Arrival -> [Arrival] -> [Arrival]
insertArrivalInOrder arr [] = [arr]
insertArrivalInOrder (Arrival _11 _12 departure _13) ((Arrival _21 _22 departure2 _23):xs) = 
    if departure > departure2 
        then (Arrival _21 _22 departure2 _23):insertArrivalInOrder (Arrival _11 _12 departure _13) xs
        else [(Arrival _11 _12 departure _13),(Arrival _21 _22 departure2 _23)] ++ xs


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



--searching api
getTrainStations :: String -> DB Train -> [Id]
getTrainStations trName tdb = stations where
    (Train _ _ stations) = head (findAllByName trName tdb)


searchConn :: String -> String -> String -> Int -> Day -> TimeOfDay -> [Arrival] -> DBS -> [[Arrival]]
searchConn lastTrain startSt endSt count day startTime initArrs (DBS sdb tdb) = ret where
    (Station _ startArrs_) = head (findAllByName startSt sdb)
    (Station _ endArrs_) = head (findAllByName endSt sdb)

    findArrival_ trainId arrs = head (filter (\arr -> getName arr == trainId) arrs)
    
    isDrivingThru_ trainId stName = elem stName (map getName (getTrainStations trainId tdb)) 
    
    getNextStations_ (x:[]) n = []
    getNextStations_ (x:xs) n  | (n == x) = xs
                      | otherwise = getNextStations_ xs n 

    ret = if count == 0 then arrivals else arrivals2 where 


        arrivals = filter (\it -> length it > 0) (
            map (\arr -> 
                if (isDrivingThru_ (getName arr) endSt && isTrainOnTimetable (getName arr) day tdb)
                    then initArrs ++ [arr, findArrival_ (getName arr) endArrs_] 
                    else []
                ) 
            properArrivals)
        
        arrivals2 = concat (map processTrain trains) 
        
        processTrain (Train tId _ tSts) = ret where
                ret = filter (\it -> length it > 0) rets
                stations = findAllByNames (getNextStations_ (map getName tSts) startSt) sdb
                rets = concat (
                        map (\(Station stId a) -> 
                                searchConn tId stId endSt (count-1) day startTime (initArrs ++ [findArrival_ tId startArrs_, findArrival_ tId a]) (DBS sdb tdb)
                            )
                        stations
                    )
                
                
        trains = filter (\tr -> not (isDrivingThru_ (getName tr) endSt || (getName tr) == lastTrain)) (findAllByNames properArrivalsNames tdb)
        properArrivals = if length initArrs > 0 
                            then filter (\(Arrival _ _ _ departureTime) ->  arrivalTime < departureTime) startArrs_
                            else filter (\(Arrival _ _ _ departureTime) ->  startTime < departureTime) startArrs_
        properArrivalsNames = map getName properArrivals
        (Arrival _ _ arrivalTime _) = last initArrs

searchAlgorithm :: String -> String -> Int -> Day -> TimeOfDay -> DBS -> [[Arrival]]
searchAlgorithm startSt endSt 0 day departureTime (DBS sdb tdb) = searchConn "" startSt endSt 0 day departureTime [] (DBS sdb tdb)
searchAlgorithm startSt endSt count day departureTime (DBS sdb tdb) = ret where
    ret =  searchAlgorithm startSt endSt (count-1) day departureTime (DBS sdb tdb) ++ searchConn "" startSt endSt count day departureTime [] (DBS sdb tdb)


search :: String -> String -> Int -> Day -> TimeOfDay -> DBS -> String
search startSt endSt count day departureTime dbs = printConnections where
    printArrs (Arrival tr st timeIn timeOut) (Arrival tr2 st2 timeIn2 timeOut2) = ret where 
        ret = line1 ++ line2 ++ line3
        line1 = getName st ++ " -> " ++ getName st2 ++ "\n"
        line2 = "Czas trwania: " ++ show timeOut ++ " - " ++ show timeIn2 ++ "\n"
        line3 = "Pociąg: " ++ getName tr ++ "\n\n"

    connections = searchAlgorithm startSt endSt count day departureTime dbs
    printConnections = concat (map (\conn -> printConnection conn) connections)

    printConnection [] = ""
    printConnection (x:y:xs) = printArrs x y ++ printConnection xs




