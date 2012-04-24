module Menu where
import IO
import Char

main = do menu

-- Funkcja menu g³ównego
menu = do 
	putStrLn " dfgdfgdf"
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

-- Funkcja wyswietlajaca menu dodawania stacji
addStationMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz dodac: "
	name <- getLine
	case name of
		"X" -> stationMenu
		"x" -> stationMenu
		otherwise -> return() 
		--case funckja name of 
		--		"1" -> do putStrLn "Dodano stacje!"
		--		"2" -> do 
		--					putStrLn "Nie udalo sie dodac stacji! Sprobuj ponownie"
		--					addStationMenu 

-- Funkcja wyswietlajaca menu usuwania stacji
eraseStationMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz usunac: "
	name <- getLine
	--if searchStation name == False 
	--	then do
	--			putStrLn "Podana stacja nie istnieje!!! Sprobuj ponownie.
	--	else
	case name of
		"X" -> stationMenu
		"x" -> stationMenu
-- DO ZROBIENIA I POPRAWIENIA -------------------------------------------------------
		otherwise -> eraseStationMenu
				

-- Funkcja wyswietlajaca menu modyfikowania stacji
modifyStationMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz modyfikowac: "
	name <- getLine
	case name of
		"X" -> stationMenu
		"x" -> stationMenu
		otherwise -> do
						putStrLn "Wprowadz nowa nazwe stacji: "
						new_name <- getLine
						case new_name of
							"X" -> modifyStationMenu
							"x" -> modifyStationMenu
							otherwise -> return()
							--case funckja name new_name of 
							--		"1" -> do putStrLn "Zmieniono nazwe stacji!"
							--		"2" -> do 
							--					putStrLn "Podana stacja nie istnieje! Sprobuj ponownie"
							--					modifyStationMenu
							--		"3" -> do 
							--					putStrLn "Nie udalo sie zmienic nazwy! Sproboj ponownie"
							--					modifyStationMenu
				

-- Funkcja wyswietlajaca opcje dla pociagu
trainMenu = do
	putStrLn " "
	putStrLn "1. Dodaj nowy pociag"
	putStrLn "2. Usun pociag"
	putStrLn "3. Modyfikuj informacje o pociagu"
	putStrLn "Wybierz opcje:"
	opt <- getLine

	case opt of
		"1" -> addTrainMenu
		"2" -> eraseTrainMenu
		"3" -> modifyTrainMenu
		otherwise -> do
			putStrLn "Wybrano zla opcje!!! Sprobuj ponownie..."
			trainMenu

i = 0
inc :: Int -> Int
inc i = i+1

-- Funkcja wyswietlajaca menu dodawania pociagu
addTrainMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, ktory chcesz dodac: "
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
		otherwise -> addStationToTrain name


-- Funkcja wyswietlajaca polecenia dodawania stacji do pociagow
addStationToTrain name = do
	putStrLn "Wprowadz pusta nazwe stacji, aby przerwac wprowadzanie danych!!!"
	putStrLn ("Podaj nazwe stacji dla pociagu: ")
	name <- getLine
	case name of
		"" -> addTrainMenu
		otherwise -> do
			--if istnieje then do
			--	putStrLn ("Podana stacja istnieje podaj inna nazwe!!!"
			--	addStationToTrain
			--	else addTimeToStation
			addTimeToStation
			--case funckja of 
			--		"1" -> do putStrLn "Dodano stacje!"
			--		"2" -> do 
			--					putStrLn "Nie udalo sie dodac stacji! Sprobuj ponownie"
			--					addTrainMenu 

-- Funkcja dodaje do stacji godziny przyjazdu i odjazdu pociagu
addTimeToStation = do
		putStrLn "Podaj godzine przyjazdu: "
		arrival <- getLine
		putStrLn "Podaj godzine odjazdu: "
--		departure <- getLine

-- Funkcja wyswietlajaca menu usuwania pociagu
eraseTrainMenu = do
	putStrLn " "
	putStrLn "Wprowadz nazwe pocagu, ktory chcesz usunac: "
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
		otherwise -> return() 
		--case funckja name of 
		--		"1" -> do putStrLn "Usunieto pociag z rozkladu jazdy!"
		--		"2" -> do 
		--					putStrLn "Podany pociag nie istnieje! Sprobuj ponownie"
		--					eraseTrainMenu 

-- Funkcja wyswietlajaca menu modyfikacji pociagu
modifyTrainMenu = do
	putStrLn " "
	putStrLn "1. Modyfikuj caly rozklad dla pociagu"
	putStrLn "2. Modyfikuj rozklad pociagu dla zadanych stacji"
	putStrLn "3. Zmien nazwe pociagu"
	name <- getLine
	case name of
		"X" -> trainMenu
		"x" -> trainMenu
		--"1" -> modifyTrainTimetable
		--"2" -> modifyPartTrainTimetable
		"3" -> modifyTrainName
		otherwise -> do
						putStrLn "Wybrano zla opcje!!! Sprobuj ponownie."
						modifyTrainMenu

-- Funkcja zmienia nazwe pociagu
modifyTrainName = do
	putStrLn " "
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz zmienic nazwe: "

				

-- Funkcja wyswietlajaca menu wyszukiwania polaczen
connectionMenu = do
	putStrLn " "
	putStrLn "Podaj stacje poczatkowa: "
	firstStation <- getLine
	case firstStation of
		"X" -> menu
		"x" -> menu
		otherwise -> do 
			putStrLn "Podaj stacje koncowa: "
			lastStation <- getLine
			case lastStation of
				"X" -> connectionMenu
				"x" -> connectionMenu
				otherwise -> do 
				case firstStation of
					"X" -> connectionMenu
					"x" -> connectionMenu
					otherwise -> do 
						putStrLn "Podaj akceptowalna liczbe przesiadek: "
						change <- getLine
						case change of
							"X" -> connectionMenu
							"x" -> connectionMenu
							otherwise -> return()
							-- funkcja firstStation lastStation change
							-- ta funkcja ktora bedzie tutaj wywolywana zwraca odpowiedniego stringa
							-- mowiacego o tym czy udalo sie znalezc polaczenie (jak tak to je wyswietla) czy nie

-- Funkcja wyswietlajaca menu dla wyszukiwania rozkladow jazdy
timetableMenu = do
	putStrLn " "
	putStrLn "Podaj nazwe stacji, ktorej chcesz zobaczyc rozklad: "
	name <- getLine
	case name of
		"X" -> menu
		"x" -> menu
		otherwise -> return()
		-- funkcja name
		-- ta funkcja ktora bedzie tutaj wywolywana zwraca odpowiedniego stringa
		-- mowiacego o tym czy udalo sie znalezc stacje (jak tak to wyswietla rozklad) czy nie