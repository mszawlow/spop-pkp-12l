import Model as M
import System.Environment

main :: IO ()

t,m :: StationDB


l = [(St "Krakow" []),(St "Warszawa" [])]
t = insertStation l empty
m = modifyStation insertArrival "Warszawa" (Arr (Tr "mieszko") "12:34" (findByName "Warszawa" t)) t


main = print m
