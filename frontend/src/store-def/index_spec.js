// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0 */
import { assert } from 'chai';
import { createStore } from 'redux';
import { combineStoreDefs, makeStoreDef } from './';

// makeStoreDef unifies action state with inital state, for simple types
() => {
  const initialState = 'hello';
  const actionsObj = (state: typeof initialState) => ({
    exampleAction() {
      (state: string);
      // $ExpectError
      (state: number);
      return state;
    },
  });
  makeStoreDef(initialState, actionsObj);
};

// makeStoreDef unifies action state with initial state, for complex object types
() => {
  const initialState = {
    counter: 1,
    todos: [
      { value: 'do something...' },
    ],
  };
  const actionsObj = (state: typeof initialState) => ({
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
  makeStoreDef(initialState, actionsObj);
};

// makeStoreDef expects state to be returned in an action
() => {
  const initialState = 'hello';
  const actionsObj = (state: typeof initialState) => ({
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
  makeStoreDef(initialState, actionsObj);
};

// makeStoreDef forbids nonexistent actions from being called
() => {
  const initialState = 'hello';
  const actionsObj = (state: typeof initialState) => ({
    exampleAction() {
      return state;
    },
  });
  const { actions, reducer } = makeStoreDef(initialState, actionsObj);
  const store = createStore(reducer);
  // $ExpectError
  store.dispatch(actions.exampleAccction());
};

// makeStoreDef forbids improper action usage
() => {
  const initialState = 'hello';
  const actionsObj = (state: typeof initialState) => ({
    exampleAction(v: void) {
      return state;
    },
    exampleAction2(v: number) {
      return state;
    },
  });
  const { actions, reducer } = makeStoreDef(initialState, actionsObj);
  const store = createStore(reducer);
  // $ExpectError
  store.dispatch(actions.exampleAction(1));
  store.dispatch(actions.exampleAction2(1));
  // $ExpectError
  store.dispatch(actions.exampleAction2('hello'));
};

describe('makeStoreDef', () => {
  it('works for identity actions', () => {
    const initialState = 'hello';
    const actionsObj = (state: typeof initialState) => ({
      exampleAction(v: void) {
        return state;
      },
    });
    const { actions, reducer } = makeStoreDef(initialState, actionsObj);
    const store = createStore(reducer);
    store.dispatch(actions.exampleAction());
    assert.equal(initialState, store.getState());
  });
  it('works for actions that modify the state', () => {
    const initialState = 'hello';
    const actionsObj = (state: typeof initialState) => ({
      exampleAction(v: void) {
        return state + state;
      },
    });
    const { actions, reducer } = makeStoreDef(initialState, actionsObj);
    const store = createStore(reducer);
    store.dispatch(actions.exampleAction());
    assert.equal('hellohello', store.getState());
  });
  it('works for multiple actions that modify the state', () => {
    const initialState = 'hello';
    const actionsObj = (state: typeof initialState) => ({
      exampleAction(v: void) {
        return 'world';
      },
      exampleAction2(v: string) {
        return v;
      },
    });
    const { actions, reducer } = makeStoreDef(initialState, actionsObj);
    const store = createStore(reducer);
    store.dispatch(actions.exampleAction());
    assert.equal('world', store.getState());
    store.dispatch(actions.exampleAction2('asdf'));
    assert.equal('asdf', store.getState());
  });
});

describe('combineStoreDefs', () => {
  it('works', () => {
    const FooStoreDef = makeStoreDef('hello', (state: string) => ({
      foo() { return state + state; },
    }));
    const BarStoreDef = makeStoreDef(1, (state: number) => ({
      bar() { return state + 2; },
    }));
    const newStoreDef = combineStoreDefs({
      foo: FooStoreDef,
      bar: BarStoreDef,
    });
    const { reducer, actions } = newStoreDef;
    const store = createStore(reducer);
    assert.deepEqual({ foo: 'hello', bar: 1 }, store.getState());
    store.dispatch(actions.foo.foo());
  });
});
