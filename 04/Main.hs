module Main where
import BSTTree
main = interact myMain

myMain s = show $ toList $ fromList (read s :: [Int])
