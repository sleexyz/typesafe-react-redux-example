// @flow
import {expect} from 'chai';
import {makeStoreDef} from  './';
import {createStore} from 'redux';

describe("makeStoreDef", () => {
  describe("Flowtype Behavior", () => {
    it("unifies action state with inital state, for simple types", () => {
      const initialState = "hello";
      const reducerObj = {
        exampleAction(state) {
          (state: string);
          // $ExpectError
          (state: number);
          return state;
        }
      };
      const {actions, reducer} = makeStoreDef(initialState, reducerObj);
      createStore(reducer);
    });

    it("expects state to be returned", () => {
      const initialState: string = "hello";
      const reducerObj = {
        exampleAction(state: string) {
          return 3;
        },
        fooAction(state) {
          return state;
        }
      };
      const {actions, reducer} = makeStoreDef(initialState, reducerObj);
      const store = createStore(reducer);
      store.dispatch(actions.exampleAction());
    });
    // it("unifies action state with inital state, for complex object types", () => {
    //   const initialState = {
    //     counter: 1,
    //     todos: [
    //       {value: "do something..."},
    //     ],
    //   }
    //   const reducerObj = (state) => ({
    //     exampleAction() {
    //       (state.counter: number);
    //       (state.todos: Array<{value: string}>);
    //       ExpectError
    //       (state.asdf: string);
    //       ExpectError
    //       (state: string);
    //       return state;
    //     }
    //   });
    //   const {actions, reducer} = makeStoreDef(initialState, reducerObj);
    //   createStore(reducer);
    // });
  });
});
