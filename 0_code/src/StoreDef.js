// @flow
import type {Reducer} from 'redux'

/*
  Moral: Redux's reducers and reducer constants should be complete opaque.
  Developers should never have to go to three places to write/update somethings.
*/

// TODO: switch to a Class interface, and use this

type ActionsType<S, O> = $ObjMap<O, <P>(v: P => S) => ((payload: P, error: ?Error) => Object)>;
type ReducerType<S, O> = Reducer<S, {type: $Keys<O>, payload: ?any, error: ?any}>;

type OutputType<S, O> = {
  actions: ActionsType<S, O>,
  reducer: ReducerType<S, O>
};

const makeActionsFromStoreDef = <S, O: {}> (makeActionsObj: S => O): ActionsType<S, O> => {
  const keys = Object.keys((makeActionsObj: any)());
  const actions = {};
  for (let i = 0; i < keys.length; i++) {
    const key = keys[i];
    actions[key] = (payload, error) => {
      return {
        type: key,
        payload: payload,
        error: error,
      };
    };
  }
  return actions;
}

const makeReducerFromStoreDef = <S, O: {}> (initialState: S, makeActionsObj: S => O): ReducerType<S, O> => {
  const actionsObj = {};
  const keys = Object.keys((makeActionsObj: any)());
  for (let i = 0; i < keys.length; i++) {
    const key = keys[i];
    actionsObj[key] = (state, payload, error) => makeActionsObj(state)[key](payload, error);
  }
  const reducer = (state = initialState, {type, payload, error}) => {
    const keys = Object.keys(actionsObj);
    for (let i = 0; i < keys.length; i++) {
      const key = keys[i];
      if (key === type) {
        return actionsObj[key](state, payload, error);
      }
    }
    return state;
  }
  return reducer;
}

export const makeStoreDef = <S, O: {}>(initialState: S, makeActionsObj: S => O): OutputType<S, O> => ({
  actions: makeActionsFromStoreDef(makeActionsObj),
  reducer: makeReducerFromStoreDef(initialState, makeActionsObj),
});