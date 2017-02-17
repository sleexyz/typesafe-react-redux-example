// @flow
import * as Typesafe from 'typesafe-redux';
import * as Todo from 'app/model/Todo';

export type Type = {
  Todo: Todo.State,
};

export const reducer = Typesafe.ReducerBuilder
  .init()
  .use(Todo.stateDef)
  .toReducer();
