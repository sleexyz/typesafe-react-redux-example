// @flow
import {assert} from 'chai';
import {makeStoreDef} from  './';
import {createStore} from 'redux';

// makeStoreDef unifies action state with inital state, for simple types
() => {
  const initialState = "hello";
  const actionsObj = (state: typeof initialState) => ({
    exampleAction() {
      (state: string);
      // $ExpectError
      (state: number);
      return state;
    }
  });
  makeStoreDef(initialState, actionsObj);
};

// makeStoreDef unifies action state with initial state, for complex object types
() => {
  const initialState = {
    counter: 1,
    todos: [
      {value: "do something..."},
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
    }
  });
  makeStoreDef(initialState, actionsObj);
};

// makeStoreDef expects state to be returned in an action
() => {
  const initialState = "hello";
  const actionsObj = (state: typeof initialState) => ({
    // $ExpectError
    exampleAction1() {
    },
    exampleAction2() {
      // $ExpectError
      return 1;
    },
  });
  makeStoreDef(initialState, actionsObj);
};

// makeStoreDef forbids nonexistent actions from being called
() => {
  const initialState = "hello";
  const actionsObj = (state: typeof initialState) => ({
    exampleAction() {
      return state;
    },
  });
  const {actions, reducer} = makeStoreDef(initialState, actionsObj);
  const store = createStore(reducer);
  // $ExpectError
  store.dispatch(actions.exampleAccction());
};

// makeStoreDef forbids improper action usage
() => {
  const initialState = "hello";
  const actionsObj = (state: typeof initialState) => ({
    exampleAction(v: void) {
      return state;
    },
    exampleAction2(v: number) {
      return state;
    },
  });
  const {actions, reducer} = makeStoreDef(initialState, actionsObj);
  const store = createStore(reducer);
  // $ExpectError
  store.dispatch(actions.exampleAction(1));
  store.dispatch(actions.exampleAction2(1));
  // $ExpectError
  store.dispatch(actions.exampleAction2('hello'));
};
