module Menu where
import IO
import Char

main = do menu

-- Funkcja menu g³ównego
menu = do 
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
		"1" -> stationMenu
		"2" -> do trainMenu
		"3" -> do connectionMenu
		"4" -> do timetableMenu
		"5" -> return()
		otherwise -> do
					putStrLn "Wybrano zla opcje!!!"
					menu

-- Funkcja wyswietlajaca opcje dla stacji
stationMenu = do
	putStrLn " "
	putStrLn "1. Dodaj stacje"
	putStrLn "2. Usun stacje"
	putStrLn "3. Modyfikuj stacje"
	putStrLn "Wybierz opcje:"
	opt <- getLine

	case opt of
		"1" -> addStationMenu
		"2" -> eraseStationMenu
		"3" -> modifyStationMenu
		"X" -> menu
		"x" -> menu
		otherwise -> do
			putStrLn "Wybrano zla opcje!!! Sprobuj ponownie..."
			stationMenu

-- Funkcje markujace dzialanie funkcji prawdziwych
searchStation "a" = True
searchStation _ = False
funkcja _ = True
funkcja2 _ = do putStrLn "Rozklad dla stacji aaa: "
searchTrain "a" = True
searchTrain _ = False

-- Funkcja wyswietlajaca menu dodawania stacji
addStationMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz dodac: "
	name <- getLine 
	case name of
		"X" -> stationMenu
		"x" -> stationMenu
		otherwise -> if searchStation name == False  then 
			if funkcja name then do
				putStrLn ("Stacja " ++ name ++ " zostala poprawnie dodana!")
				addStationMenu
			else do 
				putStrLn "Nie udalo sie dodac stacji!!! Sprobuj ponownie ..."
				addStationMenu
		else do 
			putStrLn "Podana stacja istnieje! Wpisz inna nazwe!"
			addStationMenu



-- Funkcja wyswietlajaca menu usuwania stacji
eraseStationMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz usunac: "
	name <- getLine
	case name of
		"X" -> stationMenu
		"x" -> stationMenu
		otherwise -> if searchStation name then 
			if funkcja name then do
				putStrLn ("Stacja " ++ name ++ " zostala usunieta z rozkladow!")
				eraseStationMenu
			else do 
				putStrLn "Nie udalo sie usunac stacji!!! Sprobuj ponownie ..."
				eraseStationMenu
		else do 
			putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
			eraseStationMenu

-- Funkcja wyswietlajaca menu modyfikowania stacji
modifyStationMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz modyfikowac: "
	name <- getLine
	case name of
		"X" -> stationMenu
		"x" -> stationMenu
		otherwise -> if searchStation name == False then do
			putStrLn "Podaj nowa nazwe stacji: "
			new_name <- getLine
			case new_name of
				"X" -> modifyStationMenu
				"x" -> modifyStationMenu
				otherwise -> if searchStation new_name == False then
					if funkcja name then do  -- !!!!!!!!!!!!!!!!!!!!!! funkcja name new_name
						putStrLn ("Stacja " ++ name ++ " zmienila nazwe na " ++ new_name)
						modifyStationMenu
					else do 
						putStrLn "Nie udalo sie zmienic nazwy stacji!!! Sprobuj ponownie ... "
						modifyStationMenu
				else do 
					putStrLn "Podana stacja istnieje! Wpisz inna nazwe!"
					modifyStationMenu
		else do 
			putStrLn "Podana stacja istnieje! Wpisz inna nazwe!"
			modifyStationMenu

				

-- Funkcja wyswietlajaca opcje dla pociagu
trainMenu = do
	putStrLn " "
	putStrLn "1. Dodaj nowy pociag"
	putStrLn "2. Usun pociag"
	putStrLn "3. Modyfikuj informacje o pociagu"
	putStrLn "Wybierz opcje:"
	opt <- getLine

	case opt of
		"X" -> menu
		"x" -> menu
		"1" -> addTrainMenu
		"2" -> eraseTrainMenu
		"3" -> modifyTrainMenu
		otherwise -> do
			putStrLn "Wybrano zla opcje!!! Sprobuj ponownie..."
			trainMenu

-- Funkcja wyswietlajaca menu dodawania pociagu
addTrainMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, ktory chcesz dodac: "
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
		otherwise -> if searchTrain name == False then do
			putStrLn "Podaj dni kursowania pociagu: "
			runDays <- getLine
			case runDays of
				"X" -> addTrainMenu
				"x" -> addTrainMenu
				otherwise -> if funkcja name then -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! funkcja name runDays
					addStationToTrain name "add"
				else do
					putStrLn "Nie udalo sie dodac pociagu! Sprobuj ponownie ..."
					addTrainMenu
		else do 
			putStrLn "Podany pociag istnieje! Wpisz inna nazwe!"
			addTrainMenu
				


-- Funkcja wyswietlajaca polecenia dodawania stacji do pociagow
addStationToTrain trainName mode = do
	putStrLn ("Podaj nazwe stacji dla pociagu " ++ trainName ++ " : ")
	stationName <- getLine
	case stationName of
		"X" -> if mode == "add" then addTrainMenu else modifyTrainTimetable
		"x" -> if mode == "add" then addTrainMenu else modifyTrainTimetable
		otherwise -> if searchStation stationName then do
			putStrLn ("Podaj godzine przyjazdu pociagu " ++ trainName ++ " na stacje " ++ stationName ++ ": ")
			arrival <- getLine
			case arrival of
				"X" -> if mode == "add" then addTrainMenu else modifyTrainTimetable
				"x" -> if mode == "add" then addTrainMenu else modifyTrainTimetable
				otherwise -> do
					putStrLn ("Podaj godzine odjazdu pociagu " ++ trainName ++ " ze stacji " ++ stationName ++ ": ")
					departure <- getLine
					case departure of
						"X" -> if mode == "add" then addTrainMenu else modifyTrainTimetable
						"x" -> if mode == "add" then addTrainMenu else modifyTrainTimetable
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
	putStrLn "2. Modyfikuj rozklad pociagu dla zadanych stacji"
	putStrLn "3. Zmien dni kursowania dla pociagu"
	putStrLn "4. Zmien nazwe pociagu"
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
		"1" -> modifyTrainTimetable
		"2" -> modifyPartTrainTimetable
		"3" -> modifyTrainDays
		"4" -> modifyTrainName
		otherwise -> do
						putStrLn "Wybrano zla opcje!!! Sprobuj ponownie."
						modifyTrainMenu

-- Funkcja zmienia caly rozklad jazdy dla pociagu
modifyTrainTimetable = do
	putStrLn ""
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz zmienic rozklad jazdy: "
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
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


-- DO NAPISANIA CALA FUNKCJA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
-- Funkcja zmienia wyswietla menu do podmieniania czesciowego rozkladu jazdy
modifyPartTrainTimetable = do
	putStrLn ""
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz czesciowo zmienic rozklad jazdy: "
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
		otherwise -> if searchTrain name then
			-- FUNKCJA KTORA CZYSCI WSZYSTKIE STACJE DLA POCIAGU eraseStationFromTrain name
			addStationToTrain name "modify"
		else do 
			putStrLn "Podany pociag istnieje! Wpisz inna nazwe!"
			modifyTrainTimetable


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