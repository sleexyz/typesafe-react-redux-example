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
      const splitPoint = action.type.indexOf('/');
      const expectedNamespace = action.type.substr(0, splitPoint);
      if (namespace === expectedNamespace) {
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
