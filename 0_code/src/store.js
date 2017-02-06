// @flow

import {createStore, combineReducers} from 'redux'
/* import type {Reducer} from 'redux'*/

// Supertype for all actions
type Action = {
  type: string,
  payload: any
}

type Reducer<S, A> = (S, A) => S;
type ReducerMiddleware<S, A> = (next: Reducer<S, *>) => Reducer<S, * | A>;

class ReducerBuilder<S, A> {
  reducer: Reducer<S, A>;
  constructor(reducer: (S, A) => S) {
    this.reducer = reducer;
  }
  static init<S, A>(): ReducerBuilder<S, A> {
    return new ReducerBuilder((state, action) => {
      throw new Error('Not Handled');
    });
  }
  get(): Reducer<S, A> {
    return this.reducer;
  }
  register <B>(middleware: ReducerMiddleware<S, B>): ReducerBuilder<S, A | B> {
    const newReducer = middleware(this.reducer);
    (newReducer: Reducer<S, A | B>);
    return new ReducerBuilder(newReducer);
  }
};

const handleNothing = (next) => (state, action) => {
  return next(state, action);
}

const handleHello: Reducer<*,*> = (next) => (state, action) => {
  if (action.type === 'hello') {
    return "hello"
  }
  return next(state, action);
}

const reducer = ReducerBuilder
  .init()
  .register(handleHello)
  .get();

(reducer("hello", {}));
(reducer: Reducer<string, {type: 'hello', payload: any}>);
(reducer("hello", {type: 'hello', payload: null}));
(reducer("hello", {type: '', payload: null}));

const Todos = createStore((s) => s, {
  todos: {
    '1': {
      value: 'write todomvc',
    },
    '2': {
      value: 'enjoy how meta this is',
    },
  }
});

export default Todos;

if (module.hot) {
  (module.hot: any).accept();
}
