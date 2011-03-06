module Main where

import HUnit
import Dict

tests = TestList [
		-- fromList -> toList
		TestCase $ assertEqual "for empty list returns empty list" ([]::[(Integer, Integer)]) $ toList $ fromList ([]::[(Integer, Integer)]),
		TestCase $ assertEqual "for single element list return same list" [(1,2)] $ toList $ fromList [(1,2)],
		TestCase $ assertEqual "for duplications use last" [(1,2)] $ toList $ fromList [(1,1), (1,2)],
		TestCase $ assertEqual "for long list - sort" [(1,5), (2,1), (3,2)] $ toList $ fromList [(2,1), (3,2), (1,5)],

		-- insert
		TestCase $ assertEqual "insert to empty" [(1,2)] $ toList $ insert 1 2 Empty,
		TestCase $ assertEqual "replace value" [(1,3)] $ toList $ insert 1 3 $ insert 1 2 Empty,

		-- lookup
		TestCase $ assertEqual "lookup hit" (Just 2) $ Dict.lookup 1 $ insert 1 2 Empty,
		TestCase $ assertEqual "lookup miss because of Empty" Nothing $ Dict.lookup 1 $ (Empty :: Dict Integer Integer),
		TestCase $ assertEqual "lookup miss because of not matching" Nothing $ Dict.lookup 1 $ insert 2 2 Empty,
		TestCase $ assertEqual "lookup deep hit" (Just 2) $ Dict.lookup 2 $ insert 1 1 $ insert 2 2 $ insert 3 3 Empty
	]


main = do runTestTT tests
