// @flow
import { ReducerBuilder } from 'state-def';
import * as Todo from 'app/model/Todo';

const reducer = ReducerBuilder
  .init()
  .use(Todo.stateDef)
  .toReducer();

export default reducer;
