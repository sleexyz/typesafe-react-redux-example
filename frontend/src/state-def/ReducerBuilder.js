// @flow
import type {
  $Reducer,
} from './types';

// Store-facing subtype of a StateDef:
type StateDefView<State> = {
  namespace: string,
  reducer: $Reducer<State, *>,
  initializeState: (State) => State,
};

type $ReducerBuilderConstructor<S> = {
  initialState: S,
  reducer: $Reducer<S, *>,
};

export default class ReducerBuilder<S: {}> {
  reducer: $Reducer<S, *>;
  initialState: S;
  constructor({ initialState, reducer }: $ReducerBuilderConstructor<S>) {
    this.initialState = initialState;
    this.reducer = reducer;
  }
  static init(): ReducerBuilder<{}> {
    return new ReducerBuilder({
      initialState: {},
      reducer: (x) => x,
    });
  }
  use<T: {}>(stateDefView: StateDefView<T>): ReducerBuilder<S & T> {
    const { namespace, reducer, initializeState } = stateDefView;
    // $ExpectError
    const initialState = initializeState(this.initialState);
    const newReducer = (state = initialState, action) => {
      const splitPoint = action.type.indexOf('/');
      const expectedNamespace = action.type.substr(0, splitPoint);
      if (namespace === expectedNamespace) {
        return reducer(state, action);
      }
      // $ExpectError
      return this.reducer(state, action);
    };
    // $ExpectError
    return new ReducerBuilder({
      // $ExpectError
      initialState,
      reducer: newReducer,
    });
  }
  toReducer(): $Reducer<S, *> {
    return this.reducer;
  }
}
