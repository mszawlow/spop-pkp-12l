module Menu where
import IO
import Char
import System.Time
import System.Locale
import System.Environment

-- Funkcja menu g³ównego
menu (DBS sdb tdb) = do 
	putStrLn " "
	putStrLn "--------------------------------MENU GLOWNE-----------------------------------"
	putStrLn "1. Dodaj, usun lub modyfikuj informacje o stacjach"
	putStrLn "2. Dodaj, usun lub modyfikuj informacje o pociagach"
	putStrLn "3. Wyszukaj polaczenie"
	putStrLn "4. Pokaz rozklad jazdy dla zadanej stacji"
	putStrLn "5. Zakoncz dzialanie"
	putStrLn "------------------------------------------------------------------------------"
	putStrLn "Wpisanie 'X' powoduje przerwanie akcji i cofniecie do poprzedniego menu!!!"
	putStrLn "Wybierz opcje:"
	opt <- getLine
 
	case opt of
		"1" -> stationMenu (DBS sdb tdb)
		"2" -> do trainMenu (DBS sdb tdb)
		"3" -> do connectionMenu (DBS sdb tdb)
		"4" -> do timetableMenu (DBS sdb tdb)
		"5" -> return()
		otherwise -> do
					putStrLn "Wybrano zla opcje!!!"
					menu (DBS sdb tdb)

-- Funkcja wyswietlajaca opcje dla stacji
stationMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "1. Dodaj stacje"
	putStrLn "2. Usun stacje"
	putStrLn "3. Modyfikuj stacje"
	putStrLn "Wybierz opcje:"
	opt <- getLine

	case opt of
		"1" -> addStationMenu (DBS sdb tdb)
		"2" -> eraseStationMenu (DBS sdb tdb)
		"3" -> modifyStationMenu (DBS sdb tdb)
		"X" -> menu (DBS sdb tdb)
		"x" -> menu (DBS sdb tdb)
		otherwise -> do
			putStrLn "Wybrano zla opcje!!! Sprobuj ponownie..."
			stationMenu (DBS sdb tdb)

-- Funkcje markujace dzialanie funkcji prawdziwych
searchStation "a" = True
searchStation _ = False
funkcja _ = True
funkcja2 _ = do putStrLn "Rozklad dla stacji aaa: "
searchTrain "a" = True
searchTrain _ = False

-- Funkcja wyswietlajaca menu dodawania stacji
addStationMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz dodac: "
	name <- getLine 
	case name of
		"X" -> stationMenu (DBS sdb tdb)
		"x" -> stationMenu (DBS sdb tdb)
		otherwise -> if exists name sdb == False  then
                        --jesli stacja nie istnieje to nie ma chyba mo¿liwosci zeby sie nie powiodlo, wydaje mi sie ze ten if nie jest potrzebny
			if funkcja name then do
				putStrLn ("Stacja " ++ name ++ " zostala poprawnie dodana!")
				addStationMenu (DBS (addStation name sdb) tdb)
			else do 
				putStrLn "Nie udalo sie dodac stacji!!! Sprobuj ponownie ..."
				addStationMenu (DBS sdb tdb)
		else do 
			putStrLn "Podana stacja istnieje! Wpisz inna nazwe!"
			addStationMenu (DBS sdb tdb)



-- Funkcja wyswietlajaca menu usuwania stacji
eraseStationMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz usunac: "
	name <- getLine
	case name of
		"X" -> stationMenu (DBS sdb tdb)
		"x" -> stationMenu (DBS sdb tdb)
		otherwise -> if exists name sdb then 
			if funkcja name then do --tutaj te¿, jesli wykonamy juz ta funkcje to ona nie ma prawa sie nie powiesc tak bedzie latwiej ;)
				putStrLn ("Stacja " ++ name ++ " zostala usunieta z rozkladow!")
				eraseStationMenu (DBS (eraseStation name sdb) tdb)
			else do 
				putStrLn "Nie udalo sie usunac stacji!!! Sprobuj ponownie ..."
				eraseStationMenu (DBS sdb tdb)
		else do 
			putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
			eraseStationMenu (DBS sdb tdb)

-- Funkcja wyswietlajaca menu modyfikowania stacji
modifyStationMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz modyfikowac: "
	name <- getLine
	case name of
		"X" -> stationMenu (DBS sdb tdb)
		"x" -> stationMenu (DBS sdb tdb)
		otherwise -> if exists name sdb == False then do
			putStrLn "Podaj nowa nazwe stacji: "
			new_name <- getLine
			case new_name of
				"X" -> modifyStationMenu (DBS sdb tdb)
				"x" -> modifyStationMenu (DBS sdb tdb)
				otherwise -> if exists new_name sdb == False then
					if funkcja name then do  -- !!!!!!!!!!!!!!!!!!!!!! funkcja name new_name, jw,. jak wywolamy to sie uda
						putStrLn ("Stacja " ++ name ++ " zmienila nazwe na " ++ new_name)
						modifyStationMenu (DBS (modifyStation renameStation name [new_name] sdb) tdb)
					else do 
						putStrLn "Nie udalo sie zmienic nazwy stacji!!! Sprobuj ponownie ... "
						modifyStationMenu (DBS sdb tdb)
				else do 
					putStrLn "Podana stacja istnieje! Wpisz inna nazwe!"
					modifyStationMenu (DBS sdb tdb)
		else do 
			putStrLn "Podana stacja istnieje! Wpisz inna nazwe!"
			modifyStationMenu (DBS sdb tdb)

				

-- Funkcja wyswietlajaca opcje dla pociagu
trainMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "1. Dodaj nowy pociag"
	putStrLn "2. Usun pociag"
	putStrLn "3. Modyfikuj informacje o pociagu"
	putStrLn "Wybierz opcje:"
	opt <- getLine

	case opt of
		"X" -> menu (DBS sdb tdb)
		"x" -> menu (DBS sdb tdb)
		"1" -> addTrainMenu (DBS sdb tdb)
		"2" -> eraseTrainMenu (DBS sdb tdb)
		"3" -> modifyTrainMenu (DBS sdb tdb)
		otherwise -> do
			putStrLn "Wybrano zla opcje!!! Sprobuj ponownie..."
			trainMenu (DBS sdb tdb)

-- Funkcja wyswietlajaca menu dodawania pociagu
addTrainMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, ktory chcesz dodac: "
	name <- getLine
	case name of
		"X" -> trainMenu (DBS sdb tdb)
		"x" -> trainMenu (DBS sdb tdb)
		otherwise -> if exisits name tdb == False then do
			putStrLn "Podaj dni kursowania pociagu: "
			runDays <- getLine
			case runDays of
				"X" -> addTrainMenu (DBS sdb tdb)
				"x" -> addTrainMenu (DBS sdb tdb)
				otherwise -> if funkcja name then -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! funkcja name runDays
                                        
					addStationToTrain name "add" (DBS sdb tdb)
				else do
					putStrLn "Nie udalo sie dodac pociagu! Sprobuj ponownie ..."
					addTrainMenu (DBS sdb tdb)
		else do 
			putStrLn "Podany pociag istnieje! Wpisz inna nazwe!"
			addTrainMenu (DBS sdb tdb)
				


-- Funkcja wyswietlajaca polecenia dodawania stacji do pociagow
addStationToTrain trainName mode (DBS sdb tdb) = do
	putStrLn ("Podaj nazwe stacji dla pociagu " ++ trainName ++ " : ")
	stationName <- getLine
	case stationName of
		"X" -> if mode == "add" then addTrainMenu (DBS sdb tdb) else modifyTrainTimetable (DBS sdb tdb)
		"x" -> if mode == "add" then addTrainMenu (DBS sdb tdb) else modifyTrainTimetable (DBS sdb tdb)
		otherwise -> if exists stationName sdb then do
			putStrLn ("Podaj godzine przyjazdu pociagu " ++ trainName ++ " na stacje " ++ stationName ++ ": ")
			arrival <- getLine
			case arrival of
				"X" -> if mode == "add" then addTrainMenu (DBS sdb tdb) else modifyTrainTimetable (DBS sdb tdb)
				"x" -> if mode == "add" then addTrainMenu (DBS sdb tdb) else modifyTrainTimetable (DBS sdb tdb)
				otherwise -> do
					putStrLn ("Podaj godzine odjazdu pociagu " ++ trainName ++ " ze stacji " ++ stationName ++ ": ")
					departure <- getLine
					case departure of
						"X" -> if mode == "add" then addTrainMenu (DBS sdb tdb) else modifyTrainTimetable (DBS sdb tdb)
						"x" -> if mode == "add" then addTrainMenu (DBS sdb tdb) else modifyTrainTimetable (DBS sdb tdb)
						otherwise -> if funkcja trainName then do -- !!!!!!!!!!!!!!!!!!!! funkcja trainName stationName arrival departure
							putStrLn ("Stacja " ++ stationName ++ " zostala dodana do pociagu " ++ trainName ++ ".")
							if mode == "add" then addStationToTrain trainName "add" else addStationToTrain trainName "modify"
						else do
							putStrLn "Nie udalo sie dodac stacji do pociagu! Sprobuj ponownie ..."
							if mode == "add" then addStationToTrain trainName "add" else addStationToTrain trainName "modify"
		else do 
			putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
			if mode == "add" then addStationToTrain trainName "add" else addStationToTrain trainName "modify"

-- Funkcja wyswietlajaca menu usuwania pociagu
eraseTrainMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, ktory chcesz usunac: "
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
		otherwise -> if searchTrain name then 
			if funkcja name then do
				putStrLn ("Pociag o nazwie " ++ name ++ " zostal usuniety z rozkladow.")
				eraseTrainMenu
			else do 
				putStrLn "Podany pociag nie istnieje! Wpisz inna nazwe!"
				eraseTrainMenu
		else do
			putStrLn "Podany pociag nie istnieje! Wpisz inna nazwe!"
			eraseTrainMenu
 

-- Funkcja wyswietlajaca menu modyfikacji pociagu
modifyTrainMenu = do
	putStrLn " "
	putStrLn "1. Modyfikuj caly rozklad dla pociagu"
	putStrLn "2. Dodaj stacje do pociagu lub zmien godziny przyjazdu i odjazdu pociagu dla stacji"
	putStrLn "3. Usun stacje z pociagu"
	putStrLn "4. Zmien dni kursowania dla pociagu"
	putStrLn "5. Zmien nazwe pociagu"
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
		"1" -> modifyTrainTimetable
		"2" -> addOrModifyStationForTrain
		"3" -> eraseOneStationFromTrain
		"4" -> modifyTrainDays
		"5" -> modifyTrainName
		otherwise -> do
						putStrLn "Wybrano zla opcje!!! Sprobuj ponownie."
						modifyTrainMenu

-- Funkcja zmienia caly rozklad jazdy dla pociagu
modifyTrainTimetable = do
	putStrLn ""
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz zmienic rozklad jazdy: "
	name <- getLine
	case name of
		"X" -> modifyTrainMenu
		"x" -> modifyTrainMenu
		otherwise -> if searchTrain name then do
			putStrLn "Podaj nowe dni kursowania pociagu: "
			runDays <- getLine
			case runDays of
				"X" -> modifyTrainTimetable
				"x" -> modifyTrainTimetable
				otherwise -> if funkcja name then -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! funkcja name runDays dodaje nowy pociag
					-- FUNKCJA KTORA CZYSCI WSZYSTKIE STACJE DLA POCIAGU eraseStationFromTrain name
					addStationToTrain name "modify"
				else do
					putStrLn "Nie udalo sie zmienic dni kursowania pociagu! Sprobuj ponownie ..."
					modifyTrainTimetable
		else do 
			putStrLn "Podany pociag istnieje! Wpisz inna nazwe!"
			modifyTrainTimetable

-- Funkcja dodaje stacje do pociagu
addOrModifyStationForTrain = do 	
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, dla ktorego chcesz dodac stacje badz zmienic godziny przyjazdu i odjazdu pociagu: "
	trainName <- getLine
	case trainName of
		"X" -> modifyTrainMenu
		"x" -> modifyTrainMenu
		otherwise -> if searchTrain trainName then do
			putStrLn ("Podaj nazwe stacji dla pociagu " ++ trainName ++ " : ")
			stationName <- getLine
			case stationName of
				"X" -> addOrModifyStationForTrain
				"x" -> addOrModifyStationForTrain
				otherwise -> if searchStation stationName then do
					putStrLn ("Podaj godzine przyjazdu pociagu " ++ trainName ++ " na stacje " ++ stationName ++ ": ")
					arrival <- getLine
					case arrival of
						"X" -> addOrModifyStationForTrain
						"x" -> addOrModifyStationForTrain
						otherwise -> do
							putStrLn ("Podaj godzine odjazdu pociagu " ++ trainName ++ " ze stacji " ++ stationName ++ ": ")
							departure <- getLine
							case departure of
								"X" -> addOrModifyStationForTrain
								"x" -> addOrModifyStationForTrain
								otherwise -> if funkcja trainName then do -- !!!!!!!!!!!!!!!!!!!! funkcja trainName stationName arrival departure
									putStrLn ("Stacja " ++ stationName ++ " zostala dodana lub zmodyfikowana dla pociagu " ++ trainName ++ ".")
									addOrModifyStationForTrain
								else do
									putStrLn "Nie udalo sie dodac lub zmodyfikowac stacji dla pociagu! Sprobuj ponownie ..."
									addOrModifyStationForTrain
				else do 
					putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
					addOrModifyStationForTrain
		else do 
			putStrLn "Podany pociag nie istnieje! Wpisz inna nazwe!"
			addOrModifyStationForTrain

	

-- Funkcja usuwa stacje z rozkladu jazdy pociagu
eraseOneStationFromTrain = do
	putStrLn ""
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz usunac stacje: "
	name <- getLine
	case name of
		"X" -> modifyTrainMenu
		"x" -> modifyTrainMenu
		otherwise -> if searchTrain name then do
			putStrLn ("Podaj nazwe stacji, ktora chcesz usunac z rozkladu jazdy pociagu " ++ name ++ ": ")
			stationName <- getLine
			case stationName of
				"X" -> eraseOneStationFromTrain
				"x" -> eraseOneStationFromTrain
				otherwise -> if searchTrain name then -- !!!!!!!!!!!!!! Funkcja wyszukiwania searchStationForTrain trainName stationName
					if funkcja name then do-- !!!!!!!!!!!!!!!!!!!!! funkcja name stationName (usuwajaca stacje z pociagu)
						putStrLn ("Udalo sie usunac stacje " ++ stationName ++ " z rozkladu jazdy pociagu " ++ name ++ ".")
						eraseOneStationFromTrain
					else do
						putStrLn ("Nie udalo sie usunac stacji!!! Sprobuj ponownie...")
						eraseOneStationFromTrain
				else do
					putStrLn ("Podana stacja nie istnieje lub nie nalezy do rozkladu jazdy tego pociagu!!! Sprobuj ponownie...")
					eraseOneStationFromTrain
		else do 
			putStrLn "Podany pociag nie istnieje! Wpisz inna nazwe!"
			eraseOneStationFromTrain


-- Funkcja zmienia dni kursowania dla pociagu
modifyTrainDays = do
	putStrLn ""
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz zmienic dni kursowania: "
	name <- getLine
	case name of
		"X" -> modifyTrainMenu
		"x" -> modifyTrainMenu
		otherwise -> if searchTrain name then do
			putStrLn ("Podaj dni kursowania pociagu " ++ name ++ ": ")
			new_days <- getLine
			if funkcja name then do -- !!!!!!!!!!!!!!!!!!!!!!! funkcja name new_days
				putStrLn ("Dni kursowania pociagu " ++ name ++ " zmieniono na " ++ new_days ++ ".")
				modifyTrainDays
			else do
				putStrLn "Nie udalo sie zmienic dni kursowania pociagu! Sprobuj ponownie ..."
				modifyTrainDays
		else do
			putStrLn "Podany pociag nie istnieje!!! Sprobuj ponownie ..."
			modifyTrainDays


-- Funkcja zmienia nazwe pociagu
modifyTrainName = do
	putStrLn " "
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz zmienic nazwe: "
	name <- getLine
	case name of
		"X" -> modifyTrainMenu
		"x" -> modifyTrainMenu
		otherwise -> if searchTrain name then do
			putStrLn "Podaj nowa nazwe pociagu: "
			new_name <- getLine
			case new_name of
				"X" -> modifyTrainName
				"x" -> modifyTrainName
				otherwise -> if searchTrain new_name == False then
					if funkcja name then do -- !!!!!!!!!!!!!!!!!!!!!!! funkcja name new_name
						putStrLn ("Nazwe pociagu " ++ name ++ " zmieniono na " ++ new_name ++ ".")
						modifyTrainName
					else do
						putStrLn "Nie udalo sie usunac pociagu! Sprobuj ponownie ..."
						modifyTrainName
				else do
					putStrLn "Podana pociag istnieje!!! Sprobuj ponownie ..."
					modifyTrainName
		else do
			putStrLn "Podany pociag nie istnieje!!! Sprobuj ponownie ..."
			modifyTrainName
				 
				

-- Funkcja wyswietlajaca menu wyszukiwania polaczen
connectionMenu = do
	putStrLn " "
	putStrLn "Podaj stacje poczatkowa: "
	firstStation <- getLine
	case firstStation of
		"X" -> menu
		"x" -> menu
		otherwise -> if searchStation firstStation == False then do 
			putStrLn "Podaj stacje koncowa: "
			lastStation <- getLine
			case lastStation of
				"X" -> connectionMenu
				"x" -> connectionMenu
				otherwise -> if searchStation lastStation == False then do 
					putStrLn "Podaj akceptowalna liczbe przesiadek: "
					change <- getLine
					case change of
						"X" -> connectionMenu
						"x" -> connectionMenu
						otherwise -> do
							putStrLn "Podaj dzien dla ktorego chcesz wyszukac polaczenie: "
							departureDate <- getLine
							case departureDate of
								"X" -> connectionMenu
								"x" -> connectionMenu
								otherwise -> funkcja2 departureDate -- !!!!!!!!!!!!!!!!!!!! funkcja firstStation lastStation change departureDate
									-- WYCHODZI Z PROGRAMU A CHCEMY WYSWIETLAC connectionMenu
				else do
					putStrLn "Podana stacja nie istnieje!!! Sprobuj ponownie ..."
					connectionMenu
		else do
			putStrLn "Podana stacja nie istnieje!!! Sprobuj ponownie ..."
			connectionMenu

-- Funkcja wyswietlajaca menu dla wyszukiwania rozkladow jazdy
timetableMenu = do
	putStrLn " "
	putStrLn "Podaj nazwe stacji, ktorej chcesz zobaczyc rozklad: "
	name <- getLine
	case name of
		"X" -> menu
		"x" -> menu
		otherwise -> if searchStation name == False then
			funkcja2 name
			-- WYCHODZI Z PROGRAMU A CHCEMY WYSWIETLAC timetableMenu
		else do
			putStrLn "Podana stacja nie istnieje!!! Sprobuj ponownie ..."
			timetableMenu


-- Funkcja sprawdza czy argument jest Intem
isInt [] = False
isInt [x] = if isDigit x then True
				 else False
isInt (x:xs) = if isDigit x then isInt xs
					else False
					
-- Funkcja sprawdza czy argument jest Stringiem
isString [] = False
isString [x] = if isAlpha x then True
				  else False
isString (x:xs) = if isAlpha x then isString xs
					 else False

-- !!!!!!!!!!!!!!! SPRAWDZANIE POPRAWNEJ DLUGOSCI ZEBY SIE NIE WYSYPYWALO JAK JEST ZA MALO ZNAKOW
-- Funkcja sprawdza czy argument jest godzina
isTime [] = False
isTime (a:b:c:d:e:f) = if (a>='0' && a<='1' && (isDigit b) && c==':' && d>='0' && d<='5' && (isDigit e) && f==[]) then True
							 else if (a=='2' && b>='0' && b<='3' && c==':' && d>='0' && d<='5' && (isDigit e) && f==[]) then True
							 else False
							

