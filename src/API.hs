module API where

import Model
import Char
import Data.Time hiding (Day)


-------------CREATE-------------
--Nazwa stacji -> Baza stacji -> Nowa baza stacji
--Dodaje stacje do bazy stacji, zwraca nowa baze stacji
addStation :: String -> DB Station  -> DB Station
addStation name db = insert [(Station name [])] db


--Nazwa pociagu -> Lista dni -> baza pociagow -> nowa baza pociagow
--Dodaje pociag o podanej nazwie i dniach kursowania do bazy pociagow, zwraca nowa baze
addTrain :: String -> [Day] -> DB Train -> DB Train
addTrain name days db = insert [(Train name days [])] db

--Nazwa stacji -> nazwa pociagu -> przyjazd -> odjazd -> Obydwie bazy danych -> nowe obydwie bazy danych
--Dodaje do danej stacji nowy przyjazd pociagu o podanych godzinach, do pociagu dodaje nazwe stacji ktora kolejno odwiedzi
addStationToTrain :: String -> String -> TimeOfDay -> TimeOfDay -> DBS -> DBS
addStationToTrain stName trName inTime outTime (DBS sdb tdb) = (DBS sdb' tdb') where
    sdb' = modifyStation insertArrivals stName [arrival] sdb
    arrival = (Arrival (getId train) inTime outTime)
    train = head (findAllByName trName tdb)
    tdb' = setObjects newTrains tdb
    newTrains =  map
                   (\(Train n d s) ->
                        if n == trName
                        then (Train n d (s ++ [(Id stName)]))
                        else (Train n d s))
                   (getObjects tdb)


----------------MODIFY---------------
--(Funkcja przyjmujaca stacje -> liste obiektow -> zwraca zmieniona stacje) -> Nazwa stacji -> lista obiektow do zmiany -> Baza stacji -> nowa baza stacji
--Funkcji tej moża używać do modyfikacji stacji, nazwa wyszukuje stację i dla tej stacji uruchamiana jest funkcja modyfikująca  podana jako pierwsza
modifyStation :: (Station -> [a] -> Station) -> String -> [a] -> DB Station -> DB Station
modifyStation f name items db = setObjects os' db where
    os' = map (\x -> if (getName x) == name then f x items  else x) arr
    arr = getObjects db


--Stara nazwa -> nowa nazwa -> Obydwie Bazy danych -> Nowe obydwie bazy danych
--Zmienia nazwe stacji identyfikowanej jako stara nazwa
renameTrain :: String -> String -> DBS -> DBS
renameTrain old new (DBS sdb tdb) = (DBS sdb tdb') where
    tdb' = setObjects trains' tdb
    trains = getObjects tdb
    trains' = map (\(Train id days st) -> if (old == id) then (Train new days st) else (Train id days st)) trains

--Nazwa stacji -> nazwa pociagu -> przyjazd -> odjazd -> bazy -> nowe bazy
--Zmienia czasy przyjazdow danego pociagu dla danej stacji
modifyStationToTrain :: String -> String -> TimeOfDay -> TimeOfDay -> DBS -> DBS
modifyStationToTrain stName trName inTime outTime (DBS sdb tdb) = (DBS sdb' tdb) where
    (Station name arrs) = head (findAllByName stName sdb)
    newArrs = map (\(Arrival trainId x y) -> if (getName trainId == trName) then (Arrival trainId inTime outTime) else (Arrival trainId x y)) arrs
    sdb' = modifyStation replaceArrivals stName newArrs sdb

--Nazwa pociągu -> lista dni -> Bazy -> Bazy
--Zastepuje obecna liste dni kursowania pociagu nowa lista
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


----------------DELETE--------------
--Nazwa stacji -> bazy -> bazy
--Usuwa dana stacje z bazy stacji, oraz dla kazdego pociagu usuwa ja z listy stacji pociagu
eraseStation :: String -> DBS -> DBS
eraseStation name (DBS sdb tdb) = (DBS sdb' tdb') where
    sdb' = remove name sdb
    trains = getObjects tdb
    newTrains = map (removeStationFromTrain name) trains
    tdb' = setObjects newTrains tdb

--Nazwa pociagu -> bazy -> bazy
--Usuwa przyjazdy pociagu z kazdej stacji oraz usuwa pociag z bazy pociagow
eraseTrain :: String -> DBS -> DBS
eraseTrain name (DBS sdb tdb) = (DBS (setObjects sdb' sdb) tdb') where
    sdb' = map (\station -> removeTrainArrival station (getName train)) (getObjects sdb)
    train = head (findAllByName name sdb)
    tdb' = remove name tdb

--Nazwa stacji -> nazwa pociagu -> Bazy -> bazy
--usuwa stacje z listy stacji pociagu oraz usuwa ze stacji przyjazd pociagu
eraseStationFromTrain :: String -> String -> DBS -> DBS
eraseStationFromTrain stName trName (DBS sdb tdb) = (DBS (setObjects sdb' sdb) (setObjects tdb' tdb)) where
    sdb' = map (\station -> removeTrainArrival station trName) (getObjects sdb)
    tdb' = map (\tr -> if ((getName tr) == trName) then removeStationFromTrain stName tr  else tr) (getObjects tdb)


--------------PRINT------------------------

getTimetableForStation :: String -> Day -> DBS -> String
getTimetableForStation name day (DBS sdb tdb)  = ret where
    station = head (findAllByName name sdb)
    arrivals = getArrivals station
    ret = concat (
                  map (\arr ->
                       if isTrainOnTimetable (getName arr) day tdb
                       then printArrival name arr (DBS sdb tdb)
                       else []
                      )
                  arrivals)







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
