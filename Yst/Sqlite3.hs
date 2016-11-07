module Yst.Sqlite3 (readSqlite3) where

import           Data.Maybe            (fromMaybe)
import           Database.HDBC         (disconnect, execute, getColumnNames,
                                        prepare, sFetchAllRows')
import           Database.HDBC.Sqlite3 (connectSqlite3)

import           Yst.Types             (Node (..), parseAsDate)


readSqlite3 :: FilePath -> String -> IO Node
readSqlite3 filename query = do
  conn <- connectSqlite3 filename
  stmt <- prepare conn query
  _ <- execute stmt []
  records <- sFetchAllRows' stmt
  fieldNames <- getColumnNames stmt
  disconnect conn

  return $ NList $ map (NMap .
                       zip fieldNames .
                       map (fieldToNode . fromMaybe "")) records


fieldToNode :: String -> Node
fieldToNode s =
  case parseAsDate s of
    Nothing -> NString s
    Just d  -> NDate d
