module Menu where
import Model
import API
import IO
import Char
import System.Time hiding (Day)
import Data.Time hiding (Day)
import Data.List

---------------------------------------------------------------------------------------------------------
-- Funkcja menu g³ównego---------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
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
		"5" -> do 
					writeFile "dbs.db" (show (DBS sdb tdb))
			   		return()
		otherwise -> do
					putStrLn "Wybrano zla opcje!!!"
					menu (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca opcje dla stacji----------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
stationMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "----------------------------MENU STACJI---------------------------------------"
	putStrLn "1. Dodaj stacje"
	putStrLn "2. Usun stacje"
	putStrLn "3. Modyfikuj stacje"
	putStrLn "------------------------------------------------------------------------------"
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


---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca menu dodawania stacji-----------------------------------------------------------
---------------------------------------------------------------------------------------------------------
addStationMenu (DBS sdb tdb) = do
  putStrLn " "
  putStrLn "Wprowadz nazwe stacji, ktora chcesz dodac: "
  name <- getLine
  if isString name == False then do
		  putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
		  addStationMenu (DBS sdb tdb)
	else case name of 
			"X" -> stationMenu (DBS sdb tdb)
			"x" -> stationMenu (DBS sdb tdb)
			otherwise -> if exists name sdb == False  then do 
								putStrLn ("Stacja " ++ name ++ " zostala poprawnie dodana!")
								addStationMenu (addStation name (DBS sdb tdb))
				  			else do putStrLn "Podana stacja istnieje! Wpisz inna nazwe!"
								addStationMenu (DBS sdb tdb)


---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca menu usuwania stacji------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
eraseStationMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz usunac: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			eraseStationMenu (DBS sdb tdb)
		else case name of
			"X" -> stationMenu (DBS sdb tdb)
			"x" -> stationMenu (DBS sdb tdb)
			otherwise -> if exists name sdb then do
									putStrLn ("Stacja " ++ name ++ " zostala usunieta z rozkladow!")
									eraseStationMenu (eraseStation name (DBS sdb tdb))
							 else do	putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
									eraseStationMenu (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca menu modyfikowania stacji-------------------------------------------------------
---------------------------------------------------------------------------------------------------------
modifyStationMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe stacji, ktora chcesz modyfikowac: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			modifyStationMenu (DBS sdb tdb)
		else case name of
			"X" -> stationMenu (DBS sdb tdb)
			"x" -> stationMenu (DBS sdb tdb)
			otherwise -> if exists name sdb then do
				putStrLn "Podaj nowa nazwe stacji: "
				new_name <- getLine
				if isString new_name == False then do
						putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
						modifyStationMenu (DBS sdb tdb)
					else case new_name of
						"X" -> modifyStationMenu (DBS sdb tdb)
						"x" -> modifyStationMenu (DBS sdb tdb)
						otherwise -> if exists new_name sdb == False then do
								putStrLn ("Stacja " ++ name ++ " zmienila nazwe na " ++ new_name)
								modifyStationMenu (DBS (modifyStation renameStation name [new_name] sdb) tdb)
									else do 
										putStrLn "Podana stacja istnieje! Wpisz inna nazwe!"
										modifyStationMenu (DBS sdb tdb)
					else do 
								putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
								modifyStationMenu (DBS sdb tdb)

				
---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca opcje dla pociagu---------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
trainMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "------------------------------MENU POCIAGU------------------------------------"
	putStrLn "1. Dodaj nowy pociag"
	putStrLn "2. Usun pociag"
	putStrLn "3. Modyfikuj informacje o pociagu"
	putStrLn "------------------------------------------------------------------------------"
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

---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca menu dodawania pociagu----------------------------------------------------------
---------------------------------------------------------------------------------------------------------
addTrainMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, ktory chcesz dodac: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			addTrainMenu (DBS sdb tdb)
		else case name of
			"X" -> trainMenu (DBS sdb tdb)
			"x" -> trainMenu (DBS sdb tdb)
			otherwise -> if exists name tdb == False then do
				putStrLn "Podaj dni kursowania pociagu (Mon, Tue, Wed, Thu, Fri, Sat, Sun): "
				runDays <- getLine
				case runDays of
					"X" -> addTrainMenu (DBS sdb tdb)
					"x" -> addTrainMenu (DBS sdb tdb)
					otherwise -> if isDayArray (string2array runDays) == False then do
										putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
										addTrainMenu (DBS sdb tdb)
									else addStationToTrainMenu name (addTrain name (string2dayArray (string2array runDays)) (DBS sdb tdb))
							else do 
								putStrLn "Podany pociag istnieje! Wpisz inna nazwe!"
								addTrainMenu (DBS sdb tdb)
				
---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca polecenia dodawania stacji do pociagow------------------------------------------
---------------------------------------------------------------------------------------------------------
addStationToTrainMenu trainName (DBS sdb tdb) = do
	putStrLn ("Podaj nazwe stacji dla pociagu " ++ trainName ++ " : ")
	stationName <- getLine
	if isString stationName == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			addStationToTrainMenu trainName (DBS sdb tdb)
		else case stationName of
			"X" -> addTrainMenu (DBS sdb tdb)
			"x" -> addTrainMenu (DBS sdb tdb)
			otherwise -> if exists stationName sdb then do
				putStrLn ("Podaj godzine przyjazdu pociagu " ++ trainName ++ " na stacje " ++ stationName ++ " (w formacie hh:mm): ")
				arrival <- getLine
				case arrival of
					"X" -> addTrainMenu (DBS sdb tdb)
					"x" -> addTrainMenu (DBS sdb tdb)
					otherwise -> if length arrival /= 5 then do 
										putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
										addStationToTrainMenu trainName (DBS sdb tdb)
									else if isTime arrival == False then do
										putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
										addStationToTrainMenu trainName (DBS sdb tdb)
									else do
										putStrLn ("Podaj godzine odjazdu pociagu " ++ trainName ++ " ze stacji " ++ stationName ++ " (w formacie hh:mm): ")
										departure <- getLine
										case departure of
											"X" -> addTrainMenu (DBS sdb tdb)
											"x" -> addTrainMenu (DBS sdb tdb)
											otherwise -> if length departure /= 5 then do 
																putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																addStationToTrainMenu trainName (DBS sdb tdb)
															else if isTime departure == False then do
																putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																addStationToTrainMenu trainName (DBS sdb tdb)
															else do
																putStrLn ("Stacja " ++ stationName ++ " zostala dodana do pociagu " ++ trainName ++ ".")
																addStationToTrainMenu trainName (addStationToTrain stationName trainName (string2time arrival) (string2time departure) (DBS sdb tdb))
							else do 
									putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
									addStationToTrainMenu trainName (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca menu usuwania pociagu-----------------------------------------------------------
---------------------------------------------------------------------------------------------------------
eraseTrainMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, ktory chcesz usunac: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			eraseTrainMenu (DBS sdb tdb)
		else case name of
			"X" -> trainMenu (DBS sdb tdb)
			"x" -> trainMenu (DBS sdb tdb)
			otherwise -> if exists name tdb then do
									putStrLn ("Pociag o nazwie " ++ name ++ " zostal usuniety z rozkladow.")
									eraseTrainMenu (eraseTrain name (DBS sdb tdb))
							else do
								putStrLn "Podany pociag nie istnieje! Wpisz inna nazwe!"
								eraseTrainMenu (DBS sdb tdb)
 
---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca menu modyfikacji pociagu--------------------------------------------------------
---------------------------------------------------------------------------------------------------------
modifyTrainMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "-------------------------MENU MODYFIKACJI POCIAGU-----------------------------"
	putStrLn "1. Modyfikuj caly rozklad dla pociagu"
	putStrLn "2. Dodaj stacje do pociagu"
	putStrLn "3. Zmien godziny przyjazdu i odjazdu pociagu dla zadanej stacji"
	putStrLn "4. Usun stacje z pociagu"
	putStrLn "5. Zmien dni kursowania dla pociagu"
	putStrLn "6. Zmien nazwe pociagu"
	putStrLn "------------------------------------------------------------------------------"
	putStrLn "Wybierz opcje:"
	name <- getLine
	case name of
		"X" -> trainMenu (DBS sdb tdb)
		"x" -> trainMenu (DBS sdb tdb)
		"1" -> modifyTrainTimetable (DBS sdb tdb)
		"2" -> addOneStationToTrain (DBS sdb tdb)
		"3" -> modifyStationTimetableForTrain (DBS sdb tdb)
		"4" -> eraseOneStationFromTrain (DBS sdb tdb) -- DZIALA
		"5" -> modifyTrainDaysMenu (DBS sdb tdb) -- DZIALA
		"6" -> modifyTrainName (DBS sdb tdb) -- DZIALA
		otherwise -> do
						putStrLn "Wybrano zla opcje!!! Sprobuj ponownie."
						modifyTrainMenu (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja zmienia caly rozklad jazdy dla pociagu--------------------------------------------------------
---------------------------------------------------------------------------------------------------------
modifyTrainTimetable (DBS sdb tdb) = do
	putStrLn ""
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz zmienic rozklad jazdy: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			modifyTrainTimetable (DBS sdb tdb)
		else case name of
			"X" -> modifyTrainMenu (DBS sdb tdb)
			"x" -> modifyTrainMenu (DBS sdb tdb)
			otherwise -> if exists name tdb then do
				putStrLn "Podaj nowe dni kursowania pociagu (Mon, Tue, Wed, Thu, Fri, Sat, Sun): "
				runDays <- getLine
				case runDays of
					"X" -> modifyTrainTimetable (DBS sdb tdb)
					"x" -> modifyTrainTimetable (DBS sdb tdb)
					otherwise -> if isDayArray (string2array runDays) == False then do
											putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
											modifyTrainTimetable (DBS sdb tdb)
									else modifyStationForTrain name (modifyTrainDays name (string2dayArray (string2array runDays)) (DBS sdb tdb))
							else do 
								putStrLn "Podany pociag istnieje! Wpisz inna nazwe!"
								modifyTrainTimetable (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca polecenia dodawania stacji do pociagow w trybie modyfikacji pociagu-------------
---------------------------------------------------------------------------------------------------------
modifyStationForTrain trainName (DBS sdb tdb) = do
	putStrLn ("Podaj nazwe stacji dla pociagu " ++ trainName ++ " : ")
	stationName <- getLine
	if isString stationName == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			modifyStationForTrain trainName (DBS sdb tdb)
		else case stationName of
			"X" -> modifyTrainTimetable (DBS sdb tdb)
			"x" -> modifyTrainTimetable (DBS sdb tdb)
			otherwise -> if isStationInTrain stationName trainName tdb then do
				putStrLn ("Podaj godzine przyjazdu pociagu " ++ trainName ++ " na stacje " ++ stationName ++ " (w formacie hh:mm): ")
				arrival <- getLine
				case arrival of
					"X" -> modifyTrainTimetable (DBS sdb tdb)
					"x" -> modifyTrainTimetable (DBS sdb tdb)
					otherwise -> if length arrival /= 5 then do 
										putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
										modifyStationForTrain trainName (DBS sdb tdb)
									else if isTime arrival == False then do
										putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
										modifyStationForTrain trainName (DBS sdb tdb)
									else do
										putStrLn ("Podaj godzine odjazdu pociagu " ++ trainName ++ " ze stacji " ++ stationName ++ " (w formacie hh:mm): ")
										departure <- getLine
										case departure of
											"X" -> modifyTrainTimetable (DBS sdb tdb)
											"x" -> modifyTrainTimetable (DBS sdb tdb)
											otherwise -> if length departure /= 5 then do 
																putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																modifyStationForTrain trainName (DBS sdb tdb)
															else if isTime departure == False then do
																putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																modifyStationForTrain trainName (DBS sdb tdb)
															else do
																putStrLn ("Stacja " ++ stationName ++ " zostala dodana do pociagu " ++ trainName ++ ".")
																modifyStationForTrain trainName (addStationToTrain stationName trainName (string2time arrival) (string2time departure) (DBS sdb tdb))
							else do 
									putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
									modifyStationForTrain trainName (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja dodaje stacje do pociagu----------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
addOneStationToTrain (DBS sdb tdb) = do 	
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, dla ktorego chcesz dodac stacje: "
	trainName <- getLine
	if isString trainName == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			addOneStationToTrain (DBS sdb tdb)
		else case trainName of
			"X" -> modifyTrainMenu (DBS sdb tdb)
			"x" -> modifyTrainMenu (DBS sdb tdb)
			otherwise -> if exists trainName tdb then do
				putStrLn ("Podaj nazwe stacji dla pociagu " ++ trainName ++ " : ")
				stationName <- getLine
				if isString stationName == False then do
						putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
						addOneStationToTrain (DBS sdb tdb)
					else case stationName of
						"X" -> addOneStationToTrain (DBS sdb tdb)
						"x" -> addOneStationToTrain (DBS sdb tdb)
						otherwise -> if exists stationName sdb then do
							putStrLn ("Podaj godzine przyjazdu pociagu " ++ trainName ++ " na stacje " ++ stationName ++ " (w formacie hh:dd): ")
							arrival <- getLine
							case arrival of
								"X" -> addOneStationToTrain (DBS sdb tdb)
								"x" -> addOneStationToTrain (DBS sdb tdb)
								otherwise -> if length arrival /= 5 then do 
													putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
													addOneStationToTrain (DBS sdb tdb)
												else if isTime arrival == False then do
													putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
													addOneStationToTrain (DBS sdb tdb)
												else do
													putStrLn ("Podaj godzine odjazdu pociagu " ++ trainName ++ " ze stacji " ++ stationName ++ " (w formacie hh:dd): ")
													departure <- getLine
													case departure of
														"X" -> addOneStationToTrain (DBS sdb tdb)
														"x" -> addOneStationToTrain (DBS sdb tdb)
														otherwise -> if length departure /= 5 then do 
																			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																			addOneStationToTrain (DBS sdb tdb)
																		else if isTime departure == False then do
																			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																			addOneStationToTrain (DBS sdb tdb)
																		else do
																			putStrLn ("Stacja " ++ stationName ++ " zostala dodana lub zmodyfikowana dla pociagu " ++ trainName ++ ".")
																			addOneStationToTrain (addStationToTrain stationName trainName (string2time arrival) (string2time departure) (DBS sdb tdb))
										else do 
											putStrLn "Podana stacja nie istnieje! Wpisz inna nazwe!"
											addOneStationToTrain (DBS sdb tdb)
				else do 
					putStrLn "Podany pociag nie istnieje! Wpisz inna nazwe!"
					addOneStationToTrain (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja zmienia godziny przyjazdu i odjazdu pociagu dla zadanej stacji--------------------------------
---------------------------------------------------------------------------------------------------------
modifyStationTimetableForTrain (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Wprowadz nazwe pociagu, dla ktorego chcesz zmienic godziny przyjazdu i odjazdu: "
	trainName <- getLine
	if isString trainName == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			modifyStationTimetableForTrain (DBS sdb tdb)
		else case trainName of
			"X" -> modifyTrainMenu (DBS sdb tdb)
			"x" -> modifyTrainMenu (DBS sdb tdb)
			otherwise -> if exists trainName tdb then do
				putStrLn ("Podaj nazwe stacji, dla ktorej chcesz zmienic godizny przyjazdu i odjazdu pociagu " ++ trainName ++ " : ")
				stationName <- getLine
				if isString stationName == False then do
						putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
						modifyStationTimetableForTrain (DBS sdb tdb)
					else case stationName of
						"X" -> modifyStationTimetableForTrain (DBS sdb tdb)
						"x" -> modifyStationTimetableForTrain (DBS sdb tdb)
						otherwise -> if isStationInTrain stationName trainName tdb then do
							putStrLn ("Podaj godzine przyjazdu pociagu " ++ trainName ++ " na stacje " ++ stationName ++ " (w formacie hh:mm): ")
							arrival <- getLine
							case arrival of
								"X" -> modifyStationTimetableForTrain (DBS sdb tdb)
								"x" -> modifyStationTimetableForTrain (DBS sdb tdb)
								otherwise -> if length arrival /= 5 then do 
													putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
													modifyStationTimetableForTrain (DBS sdb tdb)
												else if isTime arrival == False then do
													putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
													modifyStationTimetableForTrain (DBS sdb tdb)
												else do
													putStrLn ("Podaj godzine odjazdu pociagu " ++ trainName ++ " ze stacji " ++ stationName ++ " (w formacie hh:mm): ")
													departure <- getLine
													case departure of
														"X" -> modifyStationTimetableForTrain (DBS sdb tdb)
														"x" -> modifyStationTimetableForTrain (DBS sdb tdb)
														otherwise -> if length departure /= 5 then do 
																			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																			modifyStationTimetableForTrain (DBS sdb tdb)
																		else if isTime departure == False then do
																			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																			modifyStationTimetableForTrain (DBS sdb tdb)
																		else do
																			putStrLn ("Godziny przyjazdu i odjazdu pociagu " ++ trainName ++ " dla stacji " ++ stationName ++ " zostaly poprawnie zmienione!")
																			addOneStationToTrain (modifyStationToTrain stationName trainName (string2time arrival) (string2time departure) (DBS sdb tdb))
										else do 
											putStrLn ("Podana stacja nie istnieje lub nie nalezy do rozkladu pociagu " ++ trainName ++ "! Wpisz inna nazwe!")
											modifyStationTimetableForTrain (DBS sdb tdb)
					else do 
						putStrLn "Podany pociag nie istnieje! Wpisz inna nazwe!"
						modifyStationTimetableForTrain (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja usuwa stacje z rozkladu jazdy pociagu---------------------------------------------------------
---------------------------------------------------------------------------------------------------------
eraseOneStationFromTrain (DBS sdb tdb) = do
	putStrLn ""
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz usunac stacje: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			eraseOneStationFromTrain (DBS sdb tdb)
		else case name of
			"X" -> modifyTrainMenu (DBS sdb tdb)
			"x" -> modifyTrainMenu (DBS sdb tdb)
			otherwise -> if exists name tdb then do
				putStrLn ("Podaj nazwe stacji, ktora chcesz usunac z rozkladu jazdy pociagu " ++ name ++ ": ")
				stationName <- getLine
				if isString stationName == False then do
						putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
						eraseOneStationFromTrain (DBS sdb tdb)
					else case stationName of
						"X" -> eraseOneStationFromTrain (DBS sdb tdb)
						"x" -> eraseOneStationFromTrain (DBS sdb tdb)
						otherwise -> if isStationInTrain stationName name tdb then do
												putStrLn ("Udalo sie usunac stacje " ++ stationName ++ " z rozkladu jazdy pociagu " ++ name ++ ".")
												eraseOneStationFromTrain (eraseStationFromTrain stationName name (DBS sdb tdb))
										else do
											putStrLn ("Podana stacja nie istnieje lub nie nalezy do rozkladu jazdy tego pociagu!!! Sprobuj ponownie...")
											eraseOneStationFromTrain (DBS sdb tdb)
				else do 
					putStrLn "Podany pociag nie istnieje! Wpisz inna nazwe!"
					eraseOneStationFromTrain (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja zmienia dni kursowania dla pociagu------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
modifyTrainDaysMenu (DBS sdb tdb) = do
	putStrLn ""
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz zmienic dni kursowania: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			modifyTrainDaysMenu (DBS sdb tdb)
		else case name of
			"X" -> modifyTrainMenu (DBS sdb tdb)
			"x" -> modifyTrainMenu (DBS sdb tdb)
			otherwise -> if exists name tdb then do
				putStrLn ("Podaj dni kursowania pociagu " ++ name ++ " (Mon, Tue, Wed, Thu, Fri, Sat, Sun): ")
				new_days <- getLine
				case new_days of
					"X" -> modifyTrainMenu (DBS sdb tdb)
					"x" -> modifyTrainMenu (DBS sdb tdb)
					otherwise -> if isDayArray (string2array name) == False then do
										putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
										modifyTrainDaysMenu (DBS sdb tdb)
									else do
										putStrLn ("Dni kursowania pociagu " ++ name ++ " zmieniono na " ++ new_days ++ ".")
										modifyTrainDaysMenu (modifyTrainDays name (string2dayArray (string2array new_days)) (DBS sdb tdb))
				else do
					putStrLn "Podany pociag nie istnieje!!! Sprobuj ponownie ..."
					modifyTrainDaysMenu (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja zmienia nazwe pociagu-------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
modifyTrainName (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Podaj nazwe pociagu, dla ktorego chcesz zmienic nazwe: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			modifyTrainName (DBS sdb tdb)
		else case name of
			"X" -> modifyTrainMenu (DBS sdb tdb)
			"x" -> modifyTrainMenu (DBS sdb tdb)
			otherwise -> if exists name tdb then do
				putStrLn "Podaj nowa nazwe pociagu: "
				new_name <- getLine
				case new_name of
					"X" -> modifyTrainName (DBS sdb tdb)
					"x" -> modifyTrainName (DBS sdb tdb)
					otherwise -> if isString new_name == False then do
										putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
										modifyTrainName (DBS sdb tdb)
									else if exists new_name tdb == False then do
										putStrLn ("Nazwe pociagu " ++ name ++ " zmieniono na " ++ new_name ++ ".")
										modifyTrainName (renameTrain name new_name (DBS sdb tdb))
									else do
										putStrLn "Podana pociag istnieje!!! Sprobuj ponownie ..."
										modifyTrainName (DBS sdb tdb)
				else do
					putStrLn "Podany pociag nie istnieje!!! Sprobuj ponownie ..."
					modifyTrainName (DBS sdb tdb)
				 
				
---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca menu wyszukiwania polaczen------------------------------------------------------
---------------------------------------------------------------------------------------------------------
connectionMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Podaj stacje poczatkowa: "
	firstStation <- getLine
	if isString firstStation == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			connectionMenu (DBS sdb tdb)
		else case firstStation of
			"X" -> menu (DBS sdb tdb)
			"x" -> menu (DBS sdb tdb)
			otherwise -> if exists firstStation sdb then do 
				putStrLn "Podaj stacje koncowa: "
				lastStation <- getLine
				if isString lastStation == False then do
						putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
						connectionMenu (DBS sdb tdb)
					else case lastStation of
						"X" -> connectionMenu (DBS sdb tdb)
						"x" -> connectionMenu (DBS sdb tdb)
						otherwise -> if exists lastStation sdb then do 
							putStrLn "Podaj akceptowalna liczbe przesiadek: "
							change <- getLine
							case change of
								"X" -> connectionMenu (DBS sdb tdb)
								"x" -> connectionMenu (DBS sdb tdb)
								otherwise -> if isInt change == False then do
													putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
													connectionMenu (DBS sdb tdb)
												else do
													putStrLn "Podaj dzien dla ktorego chcesz wyszukac polaczenie (Mon, Tue, Wed, Thu, Fri, Sat, Sun): "
													departureDate <- getLine
													case departureDate of
														"X" -> connectionMenu (DBS sdb tdb)
														"x" -> connectionMenu (DBS sdb tdb)
														otherwise -> if isDayArray (string2array departureDate) == False then do
																			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																			connectionMenu (DBS sdb tdb)
																		else do	putStrLn "Podaj godzine odjazdu pociagu (w formacie hh:mm): "
																			departureTime <- getLine
																			case departureTime of
																				"X" -> connectionMenu (DBS sdb tdb)
																				"x" -> connectionMenu (DBS sdb tdb)
																				otherwise -> if length departureTime /= 5 then do 
																									putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																									connectionMenu (DBS sdb tdb)
																								else if isTime departureTime == False then do
																									putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
																									connectionMenu (DBS sdb tdb)
																								else do
																									putStrLn "------------------------------------------------------------------------------"
																									putStrLn (search firstStation lastStation (read change::Int) (read departureDate::Day) (read departureTime::TimeOfDay) (DBS sdb tdb))
																									putStrLn "------------------------------------------------------------------------------"
																									putStrLn "Nacisnij ENTER, aby wyszukac inne polaczenie ..."
																									waitForEnter <- getLine
																									connectionMenu (DBS sdb tdb)
								else do
									putStrLn "Podana stacja nie istnieje!!! Sprobuj ponownie ..."
									connectionMenu (DBS sdb tdb)
				else do
					putStrLn "Podana stacja nie istnieje!!! Sprobuj ponownie ..."
					connectionMenu (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja wyswietlajaca menu dla wyszukiwania rozkladow jazdy-------------------------------------------
---------------------------------------------------------------------------------------------------------
timetableMenu (DBS sdb tdb) = do
	putStrLn " "
	putStrLn "Podaj nazwe stacji, ktorej chcesz zobaczyc rozklad jazdy: "
	name <- getLine
	if isString name == False then do
			putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
			timetableMenu (DBS sdb tdb)
		else case name of
			"X" -> menu (DBS sdb tdb)
			"x" -> menu (DBS sdb tdb)
			otherwise -> if exists name sdb then do
				putStrLn "Podaj dzien, dla ktorego chcesz zobaczyc rozklad jazdy (Mon, Tue, Wed, Thu, Fri, Sat, Sun): "
				day <- getLine
				case day of
					"X" -> menu (DBS sdb tdb)
					"x" -> menu (DBS sdb tdb)
					otherwise -> if isDayArray (string2array day) == False then do
										putStrLn "Wprowadzono bledne dane!!! Sprobuj ponownie ..."
										timetableMenu (DBS sdb tdb)
									else do 
										putStrLn " "
										putStrLn "------------------------------------------------------------------------------"
										putStrLn (getTimetableForStation name (string2day day) (DBS sdb tdb))
										putStrLn "------------------------------------------------------------------------------"
										putStrLn "Nacisnij ENTER aby zakonczyc przegladanie rozkladu jazdy ..."
										waitForEnter <- getLine
										timetableMenu (DBS sdb tdb)
				else do
					putStrLn "Podana stacja nie istnieje!!! Sprobuj ponownie ..."
					timetableMenu (DBS sdb tdb)

---------------------------------------------------------------------------------------------------------
-- Funkcja sprawdza czy argument jest Intem--------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
isInt [] = False
isInt [x] = if isDigit x then True
				 else False
isInt (x:xs) = if isDigit x then isInt xs
					else False
					
---------------------------------------------------------------------------------------------------------
-- Funkcja sprawdza czy argument jest Stringiem----------------------------------------------------------
---------------------------------------------------------------------------------------------------------
isString [] = False
isString [x] = if isAlpha x then True
				  else False
isString (x:xs) = if isAlpha x then isString xs
					 else False

---------------------------------------------------------------------------------------------------------
-- Funkcja zmieniajaca Stringa na Time-------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
string2time string = read (string ++ ":00")::TimeOfDay

---------------------------------------------------------------------------------------------------------
-- Funkcja sprawdza czy argument jest godzina------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
isTime [] = False
isTime (a:b:c:d:e:f) = if (a>='0' && a<='1' && (isDigit b) && c==':' && d>='0' && d<='5' && (isDigit e) && f==[]) then True
							 else if (a=='2' && b>='0' && b<='3' && c==':' && d>='0' && d<='5' && (isDigit e) && f==[]) then True
							 else False

---------------------------------------------------------------------------------------------------------			
-- Funkcja zamienia dni wypisane po przecinku na tablice dni---------------------------------------------
---------------------------------------------------------------------------------------------------------
string2array :: String -> [String]	
string2array [] = []
string2array x = splitOn (==',') (stringCleaner x)

---------------------------------------------------------------------------------------------------------
-- Funkcja dzielaca Stringa na liste elementow-----------------------------------------------------------
---------------------------------------------------------------------------------------------------------
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

---------------------------------------------------------------------------------------------------------
-- Funkcja usuwa ze Stringa spacje-----------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
stringCleaner :: String -> String
stringCleaner [] = []
stringCleaner (x:xs) = if x == ' ' then stringCleaner xs 
					else x : stringCleaner xs

---------------------------------------------------------------------------------------------------------
-- Funkcja zamieniajaca Stringa na tablice Day-----------------------------------------------------------
---------------------------------------------------------------------------------------------------------
string2dayArray :: [String] -> [Day]
string2dayArray [] = []
string2dayArray (x:xs) = [read x::Day] ++ string2dayArray xs

---------------------------------------------------------------------------------------------------------
-- Funkcja zamieniajaca Stringa na Day-------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
string2day string = read string::Day

---------------------------------------------------------------------------------------------------------
-- Funkcja sprawdza czy tablica zawiera same dni---------------------------------------------------------
---------------------------------------------------------------------------------------------------------
isDayArray :: [String] -> Bool
isDayArray [] = True
isDayArray (x:xs) = if x == "Mon" || x =="Tue" || x == "Wed" || x == "Thu" || x == "Fri" || x == "Sat" || x == "Sun" then isDayArray xs
					else False