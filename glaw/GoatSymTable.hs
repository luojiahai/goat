---------------------------------------------------------------------
-- COMP90045 Programming Language Implementation                   --
-- Programming Project: Goat                                       --
--                                                                 --
-- Team: GOAT SIMULATOR                                            --
-- Members:                                                        --
--          Chenqin Zhang, Geoffrey Ka-Hoi Law, Yun Chen           --
--          733301, 759218, 760419                                 --
--          {chenqinz, glaw, yunc4}@student.unimelb.edu.au         --
---------------------------------------------------------------------


module GoatSymTable where

import GoatParser
import GoatAST
import qualified Data.Map


type HashMap = Data.Map.Map String Symbol

type Header = String

data SymTable = SymTable Header HashMap
    deriving (Show, Eq)

data Attribute = Slot Int | Type BaseType
    deriving (Show, Eq)

data Symbol = Symbol String [Attribute]
    deriving (Show, Eq)

bind :: String -> Symbol -> HashMap -> ((), HashMap)
bind key value hashMap =
    let hashMap' = Data.Map.insert key value hashMap
    in ((), hashMap')

lookup :: String -> HashMap -> (Symbol, HashMap)
lookup key hashMap = 
    case Data.Map.lookup key hashMap of
      Just value -> (value, hashMap)
      Nothing -> error $ "Undefined variable " ++ key
