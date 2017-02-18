// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0 */
import { assert } from 'chai';
import { createStore } from 'redux';
import { makeModelDef, ReducerCombiner } from './';
import * as Lens from './lens';
import * as Core from './core';

type State1 = { modelDef1: string };
type State2 = { modelDef2: number };

/*
   Flowtype tests
*/

// Works even without annotations
() => {
  const { commits: commits1, modelDef: modelDef1 } = makeModelDef({
    namespace: 'modelDef1',
    lens: (Lens.makePropertyLens('modelDef1'): Core.$Lens<{ modelDef1: string }, string>),
    initialState: 'hello',
    rawCommits: {
      replaceWith: (x: string) => () => {
        return x;
      },
      append: (x: string) => (state) => {
        return state + x;
      },
    },
  });
  const { commits: commits2, modelDef: modelDef2 } = makeModelDef({
    namespace: 'modelDef2',
    lens: (Lens.makePropertyLens('modelDef2'): Core.$Lens<{ modelDef2: number }, number>),
    initialState: 1,
    rawCommits: {
      incr: () => (state) => {
        return state + 1;
      },
    },
  });
  const reducer = ReducerCombiner
    .init()
    .use(modelDef1)
    .use(modelDef2)
    .toReducer();
  const store = createStore(reducer);
  store.getState().modelDef1;
  (store.getState().modelDef1: string);
  (store.getState().modelDef2: number);
  // $ExpectError
  (store.getState().modelDef1: null);
};

/*
   Runtime tests
*/

describe('ReducerCombiner', () => {
  const { commits: commits1, modelDef: modelDef1 } = makeModelDef({
    namespace: 'modelDef1',
    lens: (Lens.makePropertyLens('modelDef1'): Core.$Lens<{ modelDef1: string }, string>),
    initialState: 'hello',
    rawCommits: {
      replaceWith: (x: string) => () => {
        return x;
      },
      append: (x: string) => (state) => {
        return state + x;
      },
    },
  });
  const { commits: commits2, modelDef: modelDef2 } = makeModelDef({
    namespace: 'modelDef2',
    lens: (Lens.makePropertyLens('modelDef2'): Core.$Lens<{ modelDef2: number }, number>),
    initialState: 1,
    rawCommits: {
      incr: () => (state) => {
        return state + 1;
      },
    },
  });
  it('works with one modelDef', () => {
    const reducer = ReducerCombiner
      .init()
      .use(modelDef1)
      .toReducer();
    const store = createStore(reducer);
    store.dispatch(commits1.replaceWith('world'));
    assert.equal(store.getState().modelDef1, 'world');
    store.dispatch(commits1.append('world'));
    assert.equal(store.getState().modelDef1, 'worldworld');
  });

  it('works with two modelDefs', () => {
    const reducer = ReducerCombiner
      .init()
      .use(modelDef1)
      .use(modelDef2)
      .toReducer();
    const store = createStore(reducer);
    store.dispatch(commits1.replaceWith('world'));
    assert.equal(store.getState().modelDef1, 'world');
    store.dispatch(commits1.append('world'));
    assert.equal(store.getState().modelDef1, 'worldworld');
    store.dispatch(commits2.incr());
    assert.equal(store.getState().modelDef1, 'worldworld');
    assert.equal(store.getState().modelDef2, 2);
  });
});
