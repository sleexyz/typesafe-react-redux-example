import { splitPath } from './utils';

export default class ReducerCombiner {
  reducer;
  initialState;
  constructor({ initialState, reducer }) {
    this.initialState = initialState;
    this.reducer = reducer;
  }
  static init() {
    return new ReducerCombiner({
      initialState: {},
      reducer: (x) => x,
    });
  }
  use(modelDefView) {
    const { namespace, reducer, initializeState } = modelDefView;
    const initialState = initializeState(this.initialState);
    const newReducer = (state = initialState, action) => {
      const [head] = splitPath(action.type);
      if (namespace === head) {
        return reducer(state, action);
      }
      return this.reducer(state, action);
    };
    return new ReducerCombiner({
      initialState,
      reducer: newReducer,
    });
  }
  toReducer() {
    return this.reducer;
  }
}
