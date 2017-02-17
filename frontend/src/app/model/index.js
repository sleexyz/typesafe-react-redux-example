// @flow
import * as Typesafe from 'typesafe-redux';
import * as Todo from 'app/model/Todo';

export type State = {
  Todo: Todo.State,
};

export const reducer: Typesafe.$Reducer<State> =
  Typesafe.ReducerCombiner
    .init()
    .use(Todo.modelDef)
    .toReducer();
