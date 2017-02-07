// @flow
import type {Reducer} from 'redux'

/*
  Moral: Redux's reducers and reducer constants should be complete opaque.
  Developers should never have to go to three places to write/update somethings.
*/

// TODO: switch to a Class interface, and use this

type OutputType<S, O> = {
  actions: $ObjMap<O, <P>(v: (S, P) => S) => (payload: P, error: ?Error) => empty>,
  reducer: Reducer<S, {type: $Keys<O>}>
};

const makeState = <S, O: *>(initialState: S, actionsObj: O): OutputType<S, O> => ({
  actions: (() => {
    const keys = Object.keys(actionsObj);
    const ret = {};
    for (let i = 0; i < keys.length; i++) {
      const key = keys[i];
      ret[key] = (payload, error) => ({
        type: key,
        payload: payload,
        error: error
      });
    }
    return ret;
  })(),
  reducer: (state = initialState, action) => {
    const keys = Object.keys(actionsObj);
    for (let i = 0; i < keys.length; i++) {
      const key = keys[i];
      if (key === action.type) {
        return actionsObj[key](state, action);
        // return actionsObj[key].call({state}, action);
      }
    }
    return state;
  }
});

export default makeState;
