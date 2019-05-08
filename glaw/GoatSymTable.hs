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
import qualified Data.Map


type SymTable = Data.Map.Map String Symbol

data Attribute = Value String | Environment String | Type String | Operation String

data Symbol = Symbol String Attribute | SymTable