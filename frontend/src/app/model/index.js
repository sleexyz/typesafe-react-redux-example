// @flow
import { ReducerBuilder } from 'state-def';
import * as Todo from 'app/model/Todo';

export type Type = {
  Todo: Todo.State,
};

export const reducer = ReducerBuilder
  .init()
  .use(Todo.stateDef)
  .toReducer();
