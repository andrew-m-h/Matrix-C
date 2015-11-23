{-# LANGUAGE NoImplicitPrelude#-}

module Matrix.CommandLine (
  IsArgument(..),
  Argument(..),
  StdArgument,
  FlagError(..),
  Union(..),
  StdUnion,
  ErrorCode,
  parse,

  fromMandatoryINT,
  fromMandatoryFLOAT,
  fromMandatorySTRING,
  fromMandatoryOTHER,
  fromMandatoryPRESENT
                   ) where

import Prelude (Show(..), Read(..), Maybe(..), Either(..), Eq(..), Enum(..) ,Int, String, Float, Bool(..),
               not, otherwise, reads, const, error,
               (&&), (||), (++), ($))

data Union a = INT (Maybe Int)
           | FLOAT (Maybe Float)
           | STRING (Maybe String)
           | OTHER (Maybe a)
           | PRESENT Bool
             deriving (Eq)

type StdUnion = Union ()

data Argument a b = Argument {
  longcode :: String,
  shortcode :: String,
  description :: String,
  mandatory :: Bool,
  value :: Union b,
  code :: a
                }

type StdArgument a = Argument a ()

class IsArgument a where
  fromOption :: String -> Maybe a

instance IsArgument () where
  fromOption = const $ Just ()

data ErrorCode = INT_CAST_ERROR
               | FLOAT_CAST_ERROR
               | OTHER_CAST_ERROR
               | MANDATORY_ARG_MISSING
               | INVALID_FLAG
               | VALUE_NOT_PRESENT
                 deriving (Enum, Eq)
                          
data FlagError = FlagError {
  flag :: String,
  eCode :: ErrorCode
                   }

instance Show FlagError where
  show (FlagError str err) = case err of
    INT_CAST_ERROR -> "Unable to parse integer from: " ++ str
    FLOAT_CAST_ERROR -> "Unable to parse float from: " ++ str
    OTHER_CAST_ERROR -> "Unable to parse value from: " ++ str
    MANDATORY_ARG_MISSING -> "At least one required argument was missing: " ++ str
    INVALID_FLAG -> "An invalid flag was given: " ++ str
    VALUE_NOT_PRESENT -> "No value was given to the flag: " ++ str
    

parse :: (IsArgument b) => [Argument a b] -> [String] -> Either FlagError [Argument a b]
parse arguments lst = case lst of
  [] -> Right arguments
  x:y:xs -> case findArgument2 arguments of
    Left err -> Left err
    Right (args, rest) -> checkMandatory $ parse args rest
    where
      findArgument2 :: (IsArgument b) => [Argument a b] -> Either FlagError ([Argument a b], [String])
      findArgument2 argList = case argList of
        [] -> Left $ FlagError x INVALID_FLAG
        tmpArg@(Argument lcode scode descr mand val aCode):as
          | lcode == x || scode == x -> case val of
            INT _ -> case parseNum y of
              Nothing -> Left (FlagError lcode INT_CAST_ERROR)
              Just n  -> Right ((Argument lcode scode descr mand (INT $ Just n) aCode):as, xs)
            FLOAT _ -> case parseNum y of
              Nothing -> Left (FlagError lcode FLOAT_CAST_ERROR)
              Just n  -> Right ((Argument lcode scode descr mand (FLOAT $ Just n) aCode):as, xs)
            STRING _ -> Right ((Argument lcode scode descr mand (STRING $ Just y) aCode):as, xs)
            OTHER _ -> case fromOption y of
              Nothing -> Left (FlagError lcode OTHER_CAST_ERROR)
              Just v -> Right ((Argument lcode scode descr mand (OTHER $ Just v) aCode):as, xs)
            PRESENT _ -> Right ((Argument lcode scode descr mand (PRESENT True) aCode):as, y:xs)
          | otherwise -> case findArgument2 as of
            Left err -> Left err
            Right (aLst, rst) -> Right (tmpArg:aLst, rst)
  [x] -> checkMandatory $ findArgument1 arguments
    where
      findArgument1 :: (IsArgument b) => [Argument a b] -> Either FlagError [Argument a b]
      findArgument1 argList = case argList of
        [] -> Left $ FlagError x INVALID_FLAG
        a@(Argument lcode scode descr mand val aCode):as
          | lcode == x || scode == x -> case val of
            PRESENT _ -> Right (Argument lcode scode descr mand (PRESENT True) aCode:as)
            _ -> Left $ FlagError x VALUE_NOT_PRESENT
          | otherwise -> case findArgument1 as of
            Left err -> Left err
            Right ret -> Right (a:ret)

fromMandatoryINT :: Union a -> Int
fromMandatoryINT opt = case opt of
  INT Nothing -> error "A mandatory argument was not provided"
  INT (Just v) -> v
  _ -> error "CommandLine error, argument specified INT was not returned as such"

fromMandatoryFLOAT :: Union a -> Float
fromMandatoryFLOAT opt = case opt of
  FLOAT Nothing -> error "A mandatory argument was not provided"
  FLOAT (Just v) -> v
  _ -> error "CommandLine error, argument specified FLOAT was not returned as such"

fromMandatorySTRING :: Union a -> String
fromMandatorySTRING opt = case opt of
  STRING Nothing -> error "A mandatory argument was not provided"
  STRING (Just v) -> v
  _ -> error "CommandLine error, argument specified STRING was not returned as such"

fromMandatoryOTHER :: Union a -> a
fromMandatoryOTHER opt = case opt of
  OTHER Nothing -> error "A mandatory argument was not provided"
  OTHER (Just a) -> a
  _ -> error "CommandLine error, argument specified OTHER was not returned as such"

fromMandatoryPRESENT :: Union a -> Bool
fromMandatoryPRESENT opt = case opt of
  PRESENT b -> b
  _ -> error "CommandLine error, argument specified PRESENT was not returned as such"

checkMandatory :: (IsArgument b) => Either FlagError [Argument a b] -> Either FlagError [Argument a b]
checkMandatory args = case args of
  Left err -> Left err
  Right [] -> Right []
  Right (x:xs)
    | (not $ mandatory x) || present x -> case checkMandatory $ Right xs of
      Left err -> Left err
      Right argList -> Right $ x:argList
    | otherwise -> Left $ FlagError (longcode x) MANDATORY_ARG_MISSING
      where present a = case value a of
              INT Nothing -> False
              FLOAT Nothing -> False
              STRING Nothing -> False
              OTHER Nothing -> False
              PRESENT b -> b
              _ -> True

parseNum :: (Read a) => String -> Maybe a
parseNum s = case reads s of
  [(n, "")] -> Just n
  _  -> Nothing
