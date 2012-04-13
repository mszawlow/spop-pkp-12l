import Model as M
import System.Environment

main :: IO ()

main = getArgs >>= print . M.haqify . head
