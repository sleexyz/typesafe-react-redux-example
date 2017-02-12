// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0 */
import { assert } from 'chai';
import { createStore } from 'redux';
import { makeStateDef } from './';

/*
   Flowtype tests
*/

// makeStateDef's action state is consistent with inital state, for simple types
() => {
  const initialState = 'hello';
  const makeStateFunctions = (state: typeof initialState) => ({
    exampleAction() {
      (state: string);
      // $ExpectError
      (state: number);
      return state;
    },
  });
  const selectors = {};
  makeStateDef({ initialState, makeStateFunctions, selectors });
};

// makeStateDef's action state is consistent initial state, for complex object types
() => {
  const initialState = {
    counter: 1,
    todos: [
      { value: 'do something...' },
    ],
  };
  const makeStateFunctions = (state: typeof initialState) => ({
    exampleAction() {
      (state.counter: number);
      (state.todos: Array<{value: string}>);
      // $ExpectError
      (state.asdf: string);
      // $ExpectError
      (state: string);
      return state;
    },
  });
  const selectors = {};
  makeStateDef({ initialState, makeStateFunctions, selectors });
};

// makeStateDef expects state to be returned in an action
() => {
  const initialState = 'hello';
  const makeStateFunctions = (state: typeof initialState) => ({
    // $ExpectError
    exampleAction1(v: void) {
    },
    exampleAction2() {
      // $ExpectError
      return 1;
    },
    exampleAction3() {
      return state;
    },
  });
  const selectors = {};
  makeStateDef({ initialState, makeStateFunctions, selectors });
};

// makeStateDef forbids nonexistent actions from being called
() => {
  const initialState = 'hello';
  const makeStateFunctions = (state: typeof initialState) => ({
    exampleAction() {
      return state;
    },
  });
  const selectors = {};
  const { actions, reducer } = makeStateDef({ initialState, makeStateFunctions, selectors });
  const store = createStore(reducer);
  // $ExpectError
  store.dispatch(actions.exampleAccction());
};

// makeStateDef forbids improper action usage
() => {
  const initialState = 'hello';
  const makeStateFunctions = (state: typeof initialState) => ({
    exampleAction(v: void) {
      return state;
    },
    exampleAction2(v: number) {
      return state;
    },
  });
  const selectors = {};
  const { actions, reducer } = makeStateDef({ initialState, makeStateFunctions, selectors });
  const store = createStore(reducer);
  // $ExpectError
  store.dispatch(actions.exampleAction(1));
  store.dispatch(actions.exampleAction2(1));
  // $ExpectError
  store.dispatch(actions.exampleAction2('hello'));
};

// Redux's store.getState enforces the right shape
() => {
  const initialState = 'hello';
  const makeStateFunctions = (state: typeof initialState) => ({
    exampleAction(v: void) {
      return state;
    },
    exampleAction2(v: number) {
      return state;
    },
  });
  const selectors = {};
  const { actions, reducer } = makeStateDef({ initialState, makeStateFunctions, selectors });
  const store = createStore(reducer);
  (store.getState(): string);
  // $ExpectError
  (store.getState(): number);
};

// Invalid Selectors are forbidden
() => {
  const initialState = 'hello';
  type State = string;
  const makeStateFunctions = (state: State) => ({
    exampleAction(v: void) {
      return state;
    },
    exampleAction2(v: number) {
      return state;
    },
  });
  const selectors = {
    // $ExpectError
    foo: (state: number) => state,
  };
  const stateDef = makeStateDef({
    initialState,
    makeStateFunctions,
    selectors,
  });
};

// Valid Selectors must typecheck and enforce proper usage
() => {
  const initialState = 'hello';
  type State = string;
  const makeStateFunctions = (state: State) => ({
    exampleAction(v: void) {
      return state;
    },
    exampleAction2(v: number) {
      return state;
    },
  });
  const selectors = {
    head: (state: State) => state.slice(0, 1),
    length: (state: State) => state.length,
  };
  const stateDef = makeStateDef({
    initialState,
    makeStateFunctions,
    selectors,
  });
  createStore(stateDef.reducer);
  // $ExpectError
  stateDef.selectors.asdf;
  stateDef.selectors.head('foo');
  // $ExpectError
  stateDef.selectors.head(1);
};

/*
   Runtime tests
*/

describe('makeStateDef', () => {
  it('works for identity actions', () => {
    const initialState = 'hello';
    const makeStateFunctions = (state: typeof initialState) => ({
      exampleAction(v: void) {
        return state;
      },
    });
    const selectors = {};
    const { actions, reducer } = makeStateDef({ initialState, makeStateFunctions, selectors });
    const store = createStore(reducer);
    store.dispatch(actions.exampleAction());
    assert.equal(initialState, store.getState());
  });

  it('works for actions that modify the state', () => {
    const initialState = 'hello';
    const makeStateFunctions = (state: typeof initialState) => ({
      exampleAction(v: void) {
        return state + state;
      },
    });
    const selectors = {};
    const { actions, reducer } = makeStateDef({ initialState, makeStateFunctions, selectors });
    const store = createStore(reducer);
    store.dispatch(actions.exampleAction());
    assert.equal('hellohello', store.getState());
  });

  it('works for multiple actions that modify the state', () => {
    const initialState = 'hello';
    const makeStateFunctions = (state: typeof initialState) => ({
      exampleAction(v: void) {
        return 'world';
      },
      exampleAction2(v: string) {
        return v;
      },
    });
    const selectors = {};
    const { actions, reducer } = makeStateDef({ initialState, makeStateFunctions, selectors });
    const store = createStore(reducer);
    store.dispatch(actions.exampleAction());
    assert.equal('world', store.getState());
    store.dispatch(actions.exampleAction2('asdf'));
    assert.equal('asdf', store.getState());
  });
});
