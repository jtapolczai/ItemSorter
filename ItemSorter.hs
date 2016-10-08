{-# LANGUAGE OverloadedStrings,
             TypeFamilies #-}

module ItemSorter where

-- import Control.Arrow (right)
import Control.Exception (SomeException(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT(..), evalStateT, get, put)
import Data.Char (toLower)
import qualified Data.Map as M
import Data.List (sortBy, partition)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Text.Parsec as P
import System.IO (openFile, hPutStrLn, IOMode(..))
import System.REPL

-- Types
-------------------------------------------------------------------------------

type ItemName = String
type ID = Int

type Day = Int
type Month = Int
type Year = Int
data Date = Date Year Month Day
   deriving (Show, Eq, Ord)

data Item = Item {
   _itemName :: ItemName,
   _itemContainerType :: ContainerType,
   _itemQuantity :: Int,
   _itemExpiration :: Date
}

data ContainerType = Bag | Bar | Can | Jar | Pack
   deriving (Show, Eq, Ord, Enum, Bounded)

data AppState = AppState {
   _appStateDB :: CSVDB
}

-- Date
-------------------------------------------------------------------------------

-- |Creates a date out of a year, month, and day. Checks for validity.
--  The year is bounded in (1970,9999).
mkDate :: Year -> Month -> Day -> Maybe Date
mkDate y m d | isValidYear y
               && isValidMonth m
               && isValidDay y m d = Just $ Date y m d
             | otherwise = Nothing

-- |Creates a date from a possibly missing month or day.
--  The date is created with the latest possible values, e.g.
--  just passing 2012 would result in the date 2012.12.31.
mkVagueDate :: Year -> Maybe Month -> Maybe Day -> Maybe Date
mkVagueDate y (Just m) (Just d) = mkDate y m d
mkVagueDate y (Just m) Nothing = mkDate y m (daysInMonth y m)
mkVagueDate y Nothing Nothing = mkDate y 12 31

-- |Creates the latest possibly date (9999.12.31).
mkMaxDate :: Date
mkMaxDate = Date 9999 31 12

-- |Returns the current system date.
mkCurrentDate :: IO Date
mkCurrentDate = do
   (y,m,d) <- Time.toGregorian . Time.utctDay <$> Time.getCurrentTime
   return $ Date (fromIntegral y) m d

isValidYear :: Year -> Bool
isValidYear y = y >= 1970 && y < 9999

isValidMonth :: Month -> Bool
isValidMonth m = m >= 1 && m <= 12

isValidDay :: Year -> Month -> Day -> Bool
isValidDay y m d = d >= 1 && d <= daysInMonth y m

daysInMonth :: Year -> Month -> Int
daysInMonth y m | m `elem` [1,3,5,7,8,10,12] = 31
                | m `elem` [4,6,9,11] = 30
                | m == 2 = if isLeapYear y then 29 else 28
                | otherwise = 0

isLeapYear :: Year -> Bool
isLeapYear y =
   (divBy y 400) || (divBy y 4 && not (divBy y 100))
   where
      divBy x y = x `mod` y == 0

-- Parsers
-------------------------------------------------------------------------------

readItem :: P.Parsec T.Text () (ID, Item)
readItem = do
   iid <- decimal
   P.char ';'
   qty <- decimal
   P.char ';'
   contype <- readContainerType
   P.char ';'
   name <- P.many1 (P.noneOf ";")
   P.char ';'
   expiration <- readDate P.<|> readIndefinite
   P.endOfLine
   return $ (iid, Item name contype qty expiration)

readContainerType :: P.Parsec T.Text () ContainerType
readContainerType =
   P.try (P.string "bag" >> return Bag)
   P.<|> P.try (P.string "bar" >> return Bar)
   P.<|> (P.string "can" >> return Can)
   P.<|> (P.string "jar" >> return Jar)
   P.<|> (P.string "pack" >> return Pack)

readDate :: P.Parsec T.Text () Date
readDate = do
   comp1 <- decimal
   comp2 <- P.try $ P.optionMaybe readComp
   comp3 <- P.try $ P.optionMaybe readComp
   let ret = case (comp2, comp3) of
         (Just m, Just y) -> mkDate y m comp1
         (Just y, Nothing) -> mkVagueDate y (Just comp1) Nothing
         (Nothing, Nothing) -> mkVagueDate comp1 Nothing Nothing
   maybe (P.unexpected "invalid date") return ret
   where
      readComp :: P.Parsec T.Text () Int
      readComp = do P.char '.'
                    decimal

readIndefinite :: P.Parsec T.Text () Date
readIndefinite = do
   P.string "indefinite"
   return mkMaxDate

decimal :: P.Parsec T.Text () Int
decimal = read <$> P.many1 P.digit

readItems :: P.Parsec T.Text () (M.Map ID Item)
readItems = M.fromList <$> P.many readItem

-- |Pads a list to the specified length with copies of an element.
padLeft :: Int -> a -> [a] -> [a]
padLeft len x xs = padding ++ xs
   where
      padding = replicate (len - length xs) x

-- Database access
-------------------------------------------------------------------------------

class Database db where
   type DBItems db :: *
   type DBItem db :: *
   type Id db :: *
   -- |Loads the database.
   loadItems :: db -> IO db
   -- |Writes the database back. Only this operation is required to
   --  persist the data so it can be loaded back later.
   persistItems :: db -> IO ()
   -- |Filters the database.
   selectItems :: (DBItem db -> Bool) -> db -> IO (DBItems db)
   -- |Gets all items.
   getItems :: db -> IO (DBItems db)
   getItems = selectItems (const True)
   -- |Gets a single item.
   getItem :: Id db -> db -> IO (Maybe (DBItem db))
   -- |Deletes an item from the database.
   deleteItem :: db -> Id db -> IO (db)
   -- |Adds an item to the database.
   addItem :: db -> DBItem db -> IO ((db, Id db))
   -- |Updates an item in the datavase.
   updateItem :: db -> Id db -> (DBItem db -> DBItem db) -> IO (db)


-- CSV DB
-------------------------------------------------------------------------------

-- |A CSV database.
data CSVDB = CSVDB String (M.Map ID Item)

instance Database CSVDB where
   type DBItems CSVDB = M.Map ID Item
   type DBItem CSVDB = Item
   type Id CSVDB = Int


   loadItems (CSVDB fp db) = do
      items <- readItemList fp
      return $ case items of
         Left _ -> CSVDB fp db
         Right db' -> CSVDB fp db'

   persistItems (CSVDB fp db) = do
      out <- openFile fp WriteMode
      mapM_ (hPutStrLn out) . map itemAsCSV . M.toList $ db

   selectItems f (CSVDB _ db) = return $ M.filter f db
   getItem id (CSVDB _ db) = return $ M.lookup id db

   deleteItem (CSVDB fp db) id = return $ CSVDB fp (M.delete id db)
   addItem (CSVDB fp db) item = return $ (CSVDB fp (M.insert newId item db), newId)
      where
         newId = if M.null db then 0 else (+1) . fst . M.findMax $ db
   updateItem (CSVDB fp db) id f = return $ CSVDB fp (M.adjust f id db)


-- |Reads an item list from a file and returns the items in a map.
readItemList :: FilePath -> IO (Either P.ParseError (M.Map ID Item))
readItemList fp = P.parse readItems fp <$> T.readFile fp

-- |Outputs an item and an ID as a line in a CSV-file.
itemAsCSV :: (Int, Item) -> String
itemAsCSV (id, Item name contype qty exp) = mconcat
   [show id,";",show qty,";",showCT contype,";", name, showDate exp]
   where
      showCT = map toLower . show

-- SQLite DB
-------------------------------------------------------------------------------

-- |An SQLite database.
data SQLiteDB = SQLiteDB FilePath (Maybe SQL.Connection)

data ItemWithID = ItemWithID ID String ContainerType Int Date

instance SQL.FromField ContainerType where
   fromField a = case SQL.fieldData a of
      SQL.SQLText "bag" -> SQL.Ok Bag
      SQL.SQLText "bar" -> SQL.Ok Bar
      SQL.SQLText "can" -> SQL.Ok Can
      SQL.SQLText "jar" -> SQL.Ok Jar
      SQL.SQLText "pack" -> SQL.Ok Pack
      _ -> SQL.Errors [SomeException $
                       SQL.ConversionFailed "Text" "ContainerType"
                          "Couldn't parse the container type."]

instance SQL.FromField Date where
   fromField a = case SQL.fieldData a of
      SQL.SQLText t -> case P.parse readDate "" t of
         Right d -> SQL.Ok d
         Left _ -> SQL.Errors [SomeException $
                               SQL.ConversionFailed "Text" "Date"
                                  "Couldn't parse date format."]
      _ -> SQL.Errors [SomeException $
                       SQL.ConversionFailed "Text" "Date"
                          "Couldn't parse date format."]

instance SQL.ToField ContainerType where
   toField Bag = SQL.SQLText "bag"
   toField Bar = SQL.SQLText "bar"
   toField Can = SQL.SQLText "can"
   toField Jar = SQL.SQLText "jar"
   toField Pack = SQL.SQLText "pack"

instance SQL.ToField Date where
   toField (Date y m d) = SQL.SQLText $ T.pack $ mconcat
                             [show d,
                              ".",
                              show m,
                              ".",
                              show y]

instance SQL.FromRow ItemWithID where
   fromRow = ItemWithID <$> SQL.field <*> SQL.field <*> SQL.field <*> SQL.field <*> SQL.field

instance SQL.ToRow ItemWithID where
   toRow (ItemWithID id name ct qty date) = SQL.toRow (id, name, ct, qty, date)

-- App state
-------------------------------------------------------------------------------

getAppState :: IO AppState
getAppState = AppState <$> loadItems (CSVDB "food.csv" M.empty)

-- |Consume n units of an item.
consumeItem :: (Database db, DBItem db ~ Item) => Int -> Id db -> db -> IO db
consumeItem num id db = do
   item' <- getItem id db
   case item' of
      Nothing -> return db
      Just item ->
         if num >= _itemQuantity item
         then deleteItem db id
         else updateItem db id reduceQty
   where
      reduceQty x@Item{_itemQuantity=qty} = x{_itemQuantity = qty - num}

-- Main program
-------------------------------------------------------------------------------

-- |Splits an item list by expiration date. The first part contains the
--  not-yet-expired items, the second part contains the expired ones.
partitionByDate :: Date -> [(ID, Item)] -> ([(ID,Item)], [(ID,Item)])
partitionByDate today = partition f
   where
      f (_,x) = _itemExpiration x >= today

-- |Returns the collection of items, sorted by expiration date.
--  The first part contains the not-yet-expired items, the second the expired
--  ones.
itemsByExpiration :: Date -> M.Map ID Item -> ([(ID, Item)], [(ID, Item)])
itemsByExpiration today = partitionByDate today . sortBy f . M.toList
   where
      f = comparing (_itemExpiration . snd)

showItem :: Item -> String
showItem (Item name contype qty expiration) = mconcat
   [show qty, " ", name, " (", map toLower . show $ contype, "): ", showDate expiration]

showDate :: Date -> String
showDate (Date y m d) = mconcat [show d, ".", show m, ".", show y]

main :: IO ()
main = do
   as <- getAppState
   evalStateT repl as
   where
      repl :: StateT AppState IO ()
      repl = makeREPLSimple [cmdShow, cmdConsume, cmdSave]

      cmdShow :: Command (StateT AppState IO) T.Text ()
      cmdShow = makeCommand
         "show"
         (defCommandTest ["show"])
         "Shows the list of expired and still good items."
         (\_ -> do
            today <- liftIO mkCurrentDate
            items <- (_appStateDB <$> get) >>= liftIO . getItems
            let (good, expired) = itemsByExpiration today items
                show' (id, item) = mconcat [padLeft 3 ' ' (show id),
                                   " - ",
                                   showItem item]
            liftIO $ putStrLn "Expired: "
            liftIO $ putStrLn "------------------------------"
            mapM_ (liftIO . putStrLn . show') expired
            liftIO $ putStrLn "Good: "
            liftIO $ putStrLn "------------------------------"
            mapM_ (liftIO . putStrLn . show') good)

      cmdConsume :: Command (StateT AppState IO) T.Text ()
      cmdConsume = makeCommand2
         "consume"
         (defCommandTest ["consume <ID> <num>"])
         "Consumes <num> units of item <ID>"
         True
         (posNumAsker "Item ID: ")
         (posNumAsker "Number of consumed items: ")
         (\_ id num -> do
            db <- _appStateDB <$> get
            item' <- liftIO $ getItem id db
            case item' of
               Nothing -> liftIO $ putStrLn ("There's no item with the ID " ++ show id)
               Just item -> do
                  liftIO $ putStr
                         $ mconcat ["Eating ", show num, " of ", _itemName item, "... "]
                  db' <- liftIO $ consumeItem num id db
                  put $ AppState db'
                  item' <- liftIO $ getItem id db'
                  case item' of
                     Nothing -> liftIO $ putStrLn "none remain."
                     Just item ->
                        liftIO $ putStrLn
                               $ mconcat [show $ _itemQuantity item, " remain."])

      cmdSave :: Command (StateT AppState IO) T.Text ()
      cmdSave = makeCommand
         "save"
         (defCommandTest ["save"])
         "Saves the database."
         (\_ -> do
            db <- _appStateDB <$> get
            liftIO $ persistItems db)

      posNumAsker :: Applicative m => PromptMsg -> Asker' m Int
      posNumAsker pr = asker pr genericTypeError isPositive
         where
            isPositive = boolPredicate isPos (const $ genericPredicateError "Expected a natural number!")
            isPos n = pure (n >= 0)
