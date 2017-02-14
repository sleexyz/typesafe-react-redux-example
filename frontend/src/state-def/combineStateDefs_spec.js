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
  const { actions: actions1, stateDef: stateDef1 } = makeStateDef(
    'stateDef1',
    {
      replaceWith: (x: string) => (state) => {
        return { ...state, stateDef1: x };
      },
      append: (x: string) => (state) => {
        return { ...state, stateDef1: state.stateDef1 + x };
      },
    },
    { stateDef1: 'hello' },
  );
  const { actions: actions2, stateDef: stateDef2 } = makeStateDef(
    'stateDef2',
    {
      incr: () => (state) => {
        return { ...state, stateDef2: state.stateDef2 + 1 };
      },
    },
    { stateDef2: 1 },
  );
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
  const { actions: actions1, stateDef: stateDef1 } = makeStateDef(
    'stateDef1',
    {
      replaceWith: (x: string) => (state) => {
        return { ...state, stateDef1: x };
      },
      append: (x: string) => (state) => {
        return { ...state, stateDef1: state.stateDef1 + x };
      },
    },
    { stateDef1: 'hello' },
  );
  const { actions: actions2, stateDef: stateDef2 } = makeStateDef(
    'stateDef2',
    {
      incr: () => (state) => {
        return { ...state, stateDef2: state.stateDef2 + 1 };
      },
    },
    { stateDef2: 1 },
  );
  it('works with one stateDef', () => {
    const reducer = ReducerBuilder
      .init()
      .use(stateDef1)
      .toReducer();
    const store = createStore(reducer);
    store.dispatch(actions1.replaceWith('world'));
    assert.equal(store.getState().stateDef1, 'world');
    store.dispatch(actions1.append('world'));
    assert.equal(store.getState().stateDef1, 'worldworld');
  });

  it('works with two stateDefs', () => {
    const reducer = ReducerBuilder
      .init()
      .use(stateDef1)
      .use(stateDef2)
      .toReducer();
    const store = createStore(reducer);
    store.dispatch(actions1.replaceWith('world'));
    assert.equal(store.getState().stateDef1, 'world');
    store.dispatch(actions1.append('world'));
    assert.equal(store.getState().stateDef1, 'worldworld');
    store.dispatch(actions2.incr());
    assert.equal(store.getState().stateDef1, 'worldworld');
    assert.equal(store.getState().stateDef2, 2);
  });
});
