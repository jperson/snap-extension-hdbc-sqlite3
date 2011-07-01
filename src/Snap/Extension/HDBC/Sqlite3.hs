{-|

'Snap.Extension.HDBC.Sqlite3' implements a 'MonadHDBC' by using
a connection to an Sqlite3 database.

-}

module Snap.Extension.HDBC.Sqlite3
  ( hdbcInitializer
  , module Snap.Extension.HDBC
  ) where

import Control.Monad.IO.Class ( liftIO )

import Database.HDBC
import Database.HDBC.Sqlite3 ( connectSqlite3 )

import Snap.Extension ( Initializer, mkInitializer )
import Snap.Extension.HDBC
     
-- | The Initializer for 'Connection'.
-- The 'InitInfo' contains Sqlite3 connection string and 'CREATE TABLE IF NOT EXISTS' statements for the database. 
hdbcInitializer :: InitInfo -> Initializer HDBCState
hdbcInitializer info = do
    conn <- liftIO (connectSqlite3 (connString info))
    mapM_ (liftIO . flip (run conn) []) (describeTables info) >> liftIO (commit conn)
    mkInitializer $ HDBCState $ ConnWrapper conn
