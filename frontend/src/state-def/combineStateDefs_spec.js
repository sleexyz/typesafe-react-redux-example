// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0 */
import { assert } from 'chai';
import { createStore } from 'redux';
import { makeStateDef, ReducerBuilder } from './'; import type { $Reducer } from './types';

type State1 = { stateDef1: string };
type State2 = { stateDef2: number };

/*
   Flowtype tests
*/

// Works even without annotations
() => {
  const stateDef1 = makeStateDef({
    namespace: 'stateDef1',
    initializeState: (state) => ({ ...state, stateDef1: 'hello' }),
    makeStateFunctions: (state) => ({
      replaceWith(x: string) {
        return { ...state, stateDef1: x };
      },
      append(x: string) {
        return { ...state, stateDef1: state.stateDef1 + x };
      },
    }),
    selectors: {},
  });
  const stateDef2 = makeStateDef({
    namespace: 'stateDef2',
    initializeState: (state) => ({ ...state, stateDef2: 1 }),
    makeStateFunctions: (state) => ({
      incr() {
        return { ...state, stateDef2: state.stateDef2 + 1 };
      },
    }),
    selectors: {},
  });
  const reducer = ReducerBuilder
    .init()
    .use(stateDef1)
    .use(stateDef2)
    .toReducer();
  const store = createStore(reducer);
  store.getState().stateDef1;
  (store.getState().stateDef1: string);
  (store.getState().stateDef2: number);
  // $ExpectError
  (store.getState().stateDef1: number);
};

/*
   Runtime tests
*/

describe('ReducerBuilder', () => {
  const Fixtures = {
    stateDef1: makeStateDef({
      namespace: 'stateDef1',
      initializeState: (state) => ({ ...state, stateDef1: 'hello' }),
      makeStateFunctions: (state) => ({
        replaceWith(x: string) {
          return { ...state, stateDef1: x };
        },
        append(x: string) {
          return { ...state, stateDef1: state.stateDef1 + x };
        },
      }),
      selectors: {},
    }),
    stateDef2: makeStateDef({
      namespace: 'stateDef2',
      initializeState: (state) => ({ ...state, stateDef2: 1 }),
      makeStateFunctions: (state) => ({
        incr() {
          return { ...state, stateDef2: state.stateDef2 + 1 };
        },
      }),
      selectors: {},
    }),
  };
  it('works with one stateDef', () => {
    const { stateDef1 } = Fixtures;
    const reducer = ReducerBuilder
      .init()
      .use(stateDef1)
      .toReducer();
    const store = createStore(reducer);
    store.dispatch(stateDef1.actions.replaceWith('world'));
    assert.equal(store.getState().stateDef1, 'world');
    store.dispatch(stateDef1.actions.append('world'));
    assert.equal(store.getState().stateDef1, 'worldworld');
  });

  it('works with two stateDefs', () => {
    const { stateDef1, stateDef2 } = Fixtures;
    const reducer = ReducerBuilder
      .init()
      .use(stateDef1)
      .use(stateDef2)
      .toReducer();
    const store = createStore(reducer);
    store.dispatch(stateDef1.actions.replaceWith('world'));
    assert.equal(store.getState().stateDef1, 'world');
    store.dispatch(stateDef1.actions.append('world'));
    assert.equal(store.getState().stateDef1, 'worldworld');
  });
});
