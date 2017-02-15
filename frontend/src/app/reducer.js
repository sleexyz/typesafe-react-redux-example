// @flow
import { ReducerBuilder } from 'state-def';
import { stateDef as Todo } from 'app/state/Todo';

const reducer = ReducerBuilder
  .init()
  .use(Todo)
  .toReducer();

export default reducer;
