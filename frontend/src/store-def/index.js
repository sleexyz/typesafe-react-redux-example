// @flow
import type {Reducer} from 'redux'

type Action<P, O> = {type: $Keys<O>, payload: ?P, error: ?Error}

type UnaryActionCreator<P, O> = (payload: P, error?: Error) => Action<P, O>

type ActionsType<S, O> = $ObjMap<O, <P>(v: (S, P) => S) => UnaryActionCreator<P, O>>

type ReducerObj<S> = {
  [key: string]: (S, any) => S
}

type ReducerType<S, O> = Reducer<S, {type: $Keys<O>, payload: ?any, error: ?Error}>;

type OutputType<S, O> = {
  actions: ActionsType<S, O>,
  reducer: ReducerType<S, O>,
};

const makeActionsFromStoreDef = <S, O: ReducerObj<S>> (reducerObj: O): ActionsType<S, O> => {
  const keys = Object.keys(reducerObj);
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

const makeReducerFromStoreDef = <S, O: ReducerObj<S>> (initialState: S, reducerObj: O): ReducerType<S, O> => {
  const reducer = (state = initialState, {type, payload, error}) => {
    const keys = Object.keys(reducerObj);
    for (let i = 0; i < keys.length; i++) {
      const key = keys[i];
      if (key === type) {
        return reducerObj[key](state, payload, error);
      }
    }
    return state;
  }
  return reducer;
};

export const makeStoreDef = <S, O: ReducerObj<S>>(initialState: S, reducerObj: O): OutputType<S, O> => ({
  actions: makeActionsFromStoreDef(reducerObj),
  reducer: makeReducerFromStoreDef(initialState, reducerObj),
});
