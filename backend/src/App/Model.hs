{-# LANGUAGE RecordWildCards #-}

module App.Model where

import           Control.Lens hiding ((.=), elements)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

import           App.Types

-- * Mocked DB

data MockDB = MkMockDB {
  todos :: HashMap TodoId Todo,
  idCounter :: Int
}

dbEmpty :: MockDB
dbEmpty = MkMockDB {
  todos = HashMap.fromList [],
  idCounter = 0
}

dbAddTodo :: Todo -> MockDB -> (MockDB, TodoId)
dbAddTodo todo (MkMockDB oldTodos oldIdCounter) =
  (MkMockDB {..}, TodoId $ show oldIdCounter)
  where
    idCounter = oldIdCounter + 1
    todos = oldTodos
      & HashMap.insert (TodoId $ show oldIdCounter) todo

dbDeleteTodo :: TodoId -> MockDB -> (MockDB, TodoId)
dbDeleteTodo todoId (MkMockDB oldTodos idCounter) =
  (MkMockDB {..}, todoId)
  where
    todos = oldTodos
      & HashMap.delete todoId

dbUpdateTodo :: TodoId -> Todo -> MockDB -> (MockDB, Todo)
dbUpdateTodo todoId todo (MkMockDB oldTodos idCounter) =
  (MkMockDB {..}, todo)
  where
    todos = oldTodos
      & HashMap.adjust (const todo) todoId
