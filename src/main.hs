import API
import Model
import System.Environment

main :: IO ()


getTestData :: DBS
getTestData = ret where
    tdb =  DB [(Train "Mieszko" [Mon, Tue, Fri, Sat, Sun] []), (Train "Chrobry" [Mon, Tue, Fri, Sat, Sun] [])]
    tdb2 = addTrain "Walesa" [Mon,Tue,Fri,Sat] tdb
    sdb = DB [(Station "Warszawa" []),(Station "Krakow" []),(Station "Zakopane" [])]
    sdb2 = addStation "Wroclaw" sdb
    sdb3 = addStation "Bydgoszcz" sdb2
    ret = DBS sdb3 tdb2

main = ret where
    x (DBS (DB a) _) = map show a
    y (DBS _ (DB a)) = map show a
    ret = do putStrLn (concat (x getTestData) ++ concat (y getTestData))