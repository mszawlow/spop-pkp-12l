import API
import Model
import System.Environment
import System.Directory
import Data.Time hiding (Day)
import Menu

main :: IO ()
main = do
		exists <- doesFileExist "dbs.db"
		if exists	
			then do
				x <- readFile "dbs.db"
				menu (read x::DBS)
			else do
				writeFile "dbs.db" (show empty)
				menu empty
