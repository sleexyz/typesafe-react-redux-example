// @flow
import type {Reducer} from 'redux'

type ActionsObj<S> = {
  [key: string]: any => S
}

type ActionsType<S, O> = $ObjMap<O, <P>(v: P => S) => (payload: P, error?: Error) => Object>
type ReducerType<S, O> = Reducer<S, {type: $Keys<O>, payload: ?any, error: ?Error}>;

type OutputType<S, O> = {
  actions: ActionsType<S, O>,
  reducer: ReducerType<S, O>,
};

const makeActionsFromStoreDef = <S, O: ActionsObj<S>> (makeActionsObj: S => O): ActionsType<S, O> => {
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
};

const makeReducerFromStoreDef = <S, O: ActionsObj<S>> (initialState: S, makeActionsObj: S => O): ReducerType<S, O> => {
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
};

export const makeStoreDef = <S, O: ActionsObj<S>>(initialState: S, makeActionsObj: S => ActionsObj<S>): OutputType<S, O> => ({
  actions: makeActionsFromStoreDef(makeActionsObj),
  reducer: makeReducerFromStoreDef(initialState, makeActionsObj),
});
