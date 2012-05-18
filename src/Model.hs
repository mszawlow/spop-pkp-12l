module Model where

import Char
import Data.Time hiding (Day)

---------------------------------------
--data definitions---------------------
---------------------------------------
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum, Read)

-- nazwa dni stacje
data Train = Train String [Day] [Id] deriving (Show,Read)

--konstruktor: St name [lista pociagow]
data Station = Station String [Arrival] deriving (Show,Read)

--konstruktor: Arr pociagId czasPrzyjazdu czasOdjazdu
data Arrival = Arrival Id Id TimeOfDay TimeOfDay deriving (Show,Read)

data (Named a) => DB a = DB [a] deriving (Show,Read)

data DBS = DBS (DB Station) (DB Train) deriving (Show,Read)

data Id = Id String deriving (Show,Read)


----------------------------------------
--type classes definitions--------------
----------------------------------------
class Named d  where
    getName :: d -> String


class Database d where
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

instance Eq Day where
    c == c' = fromEnum c == fromEnum c'


----------------PRINT-----------------------
--Dla nazwy stacji i arrivala wyswietla arrival kolejnej stacji.
--Czyli wyswietla kolejna stacje pociagu.
printNextArrival :: String -> Arrival -> DBS -> String
printNextArrival stName arr (DBS sdb tdb) = ret where
    (Station _ arrs) = head (findAllByName stName sdb)
    ret = printArrival arr ++ getNext stations stName
    (Train _ _ stations) = head (findAllByName (getName arr) tdb)
    getNext (x:[]) n = "Ostatnia stacja\n"
    getNext (x:xs) n  | (n == getName x) = getName (head xs) ++ "\n"
                      | otherwise = getNext xs n

--Wyswietla trase pociagu
printTrain :: Train -> String
printTrain (Train name days stations) = ret where
                    ret = "Pociag [" ++ name ++ "]\n" ++ join (names stations) " -> "  ++"\n"
                    names a = map (\it -> getName it) a

--wyswietla stacje i jej rozklad
printStation :: Station -> String
printStation (Station name arrs) = "Stacja [" ++ name ++ "]\n" ++ concat (map show arrs)

--wyswietla obiekt arrival
printArrival :: Arrival -> String
printArrival (Arrival tr st timeIn timeOut) = getName st ++ " : " ++ show timeIn ++ " " ++ show timeOut ++ " " ++ getName tr ++ " -> "

--pomocnicza funkcja ktora laczy tablice stringow podanym separatorem
join :: [String] -> String -> String
join [] separator = ""
join (x:[]) separator = x
join (x:xs) separator = concat ( [x] ++ [separator] ++ [join xs separator])

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

------------------GETTERS---------------------
--zwraca liste przyjazdow dla danej stacji
getArrivals :: Station -> [Arrival]
getArrivals (Station name arrs) = arrs

--zwraca dni kursowania pociagu
getDays :: Train -> [Day]
getDays (Train _ days _) = days

--dla obiektu klasy named tworzy jego id z pola name
getId :: (Named a) => a -> Id
getId a = (Id (getName a))

--zwraca pusta baze danych
empty :: DBS
empty = DBS (DB []) (DB [])

--zwraca stacje pociagu wyszukanego po nazwie z bazy
getTrainStations :: String -> DB Train -> [Id]
getTrainStations trName tdb = stations where
    (Train _ _ stations) = head (findAllByName trName tdb)

------------------INSERT------------------------
--wstawia arrival tak aby były posortowane wg czasu przyjazdu, funkcja używana przez modifyStation
insertArrivals :: Station -> [Arrival] -> Station
insertArrivals (Station name arrs) arr = Station name (insertArrivalInOrder (head arr) arrs)

insertArrivalInOrder :: Arrival -> [Arrival] -> [Arrival]
insertArrivalInOrder arr [] = [arr]
insertArrivalInOrder (Arrival _11 _12 departure _13) ((Arrival _21 _22 departure2 _23):xs) = 
    if departure > departure2 
        then (Arrival _21 _22 departure2 _23):insertArrivalInOrder (Arrival _11 _12 departure _13) xs
        else [(Arrival _11 _12 departure _13),(Arrival _21 _22 departure2 _23)] ++ xs

-------------------MODIFY----------------------
--Rowniez funkcja uzywana przez modifyStation
replaceArrivals :: Station -> [Arrival] -> Station
replaceArrivals (Station name arrs) arr = Station name arr

--jak wyżej. 
renameStation :: Station -> [String] -> Station
renameStation (Station _ arrs) names = (Station (head names) arrs)

--------------------REMOVE---------------------
--usuwa stacje z o podanej nazwie z pociagu
removeStationFromTrain :: String -> Train -> Train
removeStationFromTrain stId (Train name days st) = (Train name days stations) where
    stations = concat (map (\it -> if getName it == name then [] else [it]) st)

--usuwa ze stacji przyjazd pociagu
removeTrainArrival :: Station -> String -> Station
removeTrainArrival (Station name arrs) trainName = (Station name arrs') where
    arrs' = concat (map (\it -> if trainName == getName it then [] else [it]) arrs)



--------------------------Searching api, magic inside--
--glowna funkcja szukajaca - reszta to wrappery, szuka polaczenia dla zadanej liczby przesiadek
--i tylko tej liczby
searchConn :: String -> String -> String -> Int -> Day -> TimeOfDay -> [Arrival] -> DBS -> [[Arrival]]
searchConn lastTrain startSt endSt count day startTime initArrs (DBS sdb tdb) = ret where
    --poczatkowe i koncowe przyjazdy, czyli jakie pociagi moga wchodzic w gre
    (Station _ startArrs_) = head (findAllByName startSt sdb)
    (Station _ endArrs_) = head (findAllByName endSt sdb)
    --funkcja pomocnicza. szuka przyjazdu pociagu w liscie przyjazdow
    findArrival_ trainId arrs = head (filter (\arr -> getName arr == trainId) arrs)
    --funkcja pomocnicza. sprawdza czy pociag jedzie przez dana stacje
    isDrivingThru_ trainId stName = elem stName (map getName (getTrainStations trainId tdb)) 
    --funkcja pomocnicze. pobiera wszystkie kolejne stacje danego pociagu
    getNextStations_ (x:[]) n = []
    getNextStations_ (x:xs) n  | (n == x) = xs
                      | otherwise = getNextStations_ xs n 

    --glowna funkcja, w zależności od kroku algorytmu zwraca arrivals lub arrivals2
    ret = if count == 0 then arrivals else arrivals2 where 
        --jeśli przesiadek jest 0 to dla properArrivals czyli listy przyjazdow pociagow, sprawdzamy czy kazdy 
        --pociag zatrzymuje sie na szukanej stacji koncowej
        arrivals = filter (\it -> length it > 0) (
            map (\arr -> 
                if (isDrivingThru_ (getName arr) endSt && isTrainOnTimetable (getName arr) day tdb)
                    --jesli sie zatrzymuje to dodaj do poprzednich przesiadek ten start i koniec
                    then initArrs ++ [arr, findArrival_ (getName arr) endArrs_] 
                    --jesli nie to tym pociagniem nie dojedziemy
                    else []
                ) 
            properArrivals)
        
        arrivals2 = concat (map processTrain trains) 
        --dla kazdego pociagu sprawdzamy czy z jego kolejnych stacji da sie dojechać do naszej koncowej
        --ale z mniejsza o 1 iloscia przesiadek
        processTrain (Train tId _ tSts) = ret where
                --wywalamy puste listy - czyli slepe zaulki algorytmu
                ret = filter (\it -> length it > 0) rets
                --kolejne stacje pociagu
                stations = findAllByNames (getNextStations_ (map getName tSts) startSt) sdb
                --dla kolejnych stacji szukamy polaczenia - traktujemy to jako przesiadka
                rets = concat (
                        map (\(Station stId a) -> 
                                searchConn tId stId endSt (count-1) day startTime (initArrs ++ [findArrival_ tId startArrs_, findArrival_ tId a]) (DBS sdb tdb)
                            )
                        stations
                    )
                
        --funkcja szuka pociagow ktore jada z tej stacji oraz nie sa poprzednim pociagiem (bo sie przesiadamy) oraz 
        --takie ktore nie jada do stacji koncowej - bo mamy niewykorzystane przesiadki jeszcze        
        trains = filter (\tr -> not (isDrivingThru_ (getName tr) endSt || (getName tr) == lastTrain)) (findAllByNames properArrivalsNames tdb)
        --filtruje przyjazdy tak aby nie szukal przesiadek kiedy odjazd jest wczesniej niz przyjazd
        properArrivals = if length initArrs > 0 
                            then filter (\(Arrival _ _ _ departureTime) ->  arrivalTime < departureTime) startArrs_
                            else filter (\(Arrival _ _ _ departureTime) ->  startTime < departureTime) startArrs_
        properArrivalsNames = map getName properArrivals
        (Arrival _ _ arrivalTime _) = last initArrs

--wrapper dla algorytmu, uruchamia go dla danej liczby przesiadek oraz dekrementuje licznik.
--w efekcie dostajemy wszystkie możliwości liczby przesiadek do zadanej ilości wlacznie
searchAlgorithm :: String -> String -> Int -> Day -> TimeOfDay -> DBS -> [[Arrival]]
searchAlgorithm startSt endSt 0 day departureTime (DBS sdb tdb) = searchConn "" startSt endSt 0 day departureTime [] (DBS sdb tdb)
searchAlgorithm startSt endSt count day departureTime (DBS sdb tdb) = ret where
    ret =  searchAlgorithm startSt endSt (count-1) day departureTime (DBS sdb tdb) ++ searchConn "" startSt endSt count day departureTime [] (DBS sdb tdb)





