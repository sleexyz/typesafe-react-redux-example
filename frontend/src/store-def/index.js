// @flow
/* eslint max-len: 0 */
import type { Reducer } from 'redux';

// We use ActionsObj<S, O> in order to preserve the type variable O
// which carries the payload type information,
// which would otherwise be lost in _ActionsObj<S>

type _ActionsObj<S> = {
  [key: string]: * => S,
};

type ActionsObj<S, O> = O & _ActionsObj<S>;

type Action<P, O> = { type: $Keys<O>, payload?: P, error?: Error };

type ActionsType<S, O> = $ObjMap<O, <P>(v: P => S) => (payload?: P, error?: Error) => Action<P, O>>;

// eslint-disable-next-line flowtype/no-weak-types
type ReducerType<S, O> = Reducer<S, {type: $Keys<O>, payload?: any, error?: Error}>;

type OutputType<S, O> = {
  initialState: S,
  actions: ActionsType<S, O>,
  reducer: ReducerType<S, O>,
};

const makeActionsFromStoreDef = <S, O: _ActionsObj<S>>
  (makeActionsObj: S => ActionsObj<S, O>): ActionsType<S, O> => {
  const keys = Object.keys((makeActionsObj: any)()); // eslint-disable-line flowtype/no-weak-types
  const actions = {};
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    actions[key] = (payload, error) => ({
      error,
      payload,
      type: key,
    });
  }
  return actions;
};

const makeReducerFromStoreDef = <S, O: _ActionsObj<S>> (initialState: S, makeActionsObj: S => ActionsObj<S, O>): ReducerType<S, O> => {
  const actionsObj = {};
  const keys = Object.keys((makeActionsObj: any)()); // eslint-disable-line flowtype/no-weak-types
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    actionsObj[key] = (state, payload, error) => makeActionsObj(state)[key](payload, error);
  }
  const reducer = (state = initialState, { type, payload, error }) => {
    for (let i = 0; i < keys.length; i += 1) {
      const key = keys[i];
      if (key === type) {
        return actionsObj[key](state, payload, error);
      }
    }
    return state;
  };
  return reducer;
};

export const makeStoreDef = <S, O: _ActionsObj<S>> (initialState: S, makeActionsObj: S => ActionsObj<S, O>): OutputType<S, O> => ({
  initialState,
  actions: makeActionsFromStoreDef(makeActionsObj),
  reducer: makeReducerFromStoreDef(initialState, makeActionsObj),
});

type StoreDefMap = {
  [key: string]: {
    actions: {},
    reducer: Function,
  },
};

type ExtractActions = <A>(v: { action: A }) => A;

// TODO: namespace action constants
const makeNamespacedActionsFromStoreDefMap = <O: {}>(storeDefMap: O): $ObjMap<O, ExtractActions> => {
  const namespacedActions = {};
  const keys = Object.keys(storeDefMap);
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    namespacedActions[key] = storeDefMap[key].actions;
  }
  return namespacedActions;
};

type ExtractState = <S>(v: { reducer: (S, any) => S }) => S;

const makeReducerFromStoreDefMap = <O: {}>(storeDefMap: O) => (rawState: $ObjMap<O, ExtractState>, action: any) => {
  let state = rawState;
  const keys = Object.keys(storeDefMap);
  if (state == null) {
    state = {};
    for (let i = 0; i < keys.length; i += 1) {
      const key = keys[i];
      state[key] = storeDefMap[key].initialState;
    }
  }
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    if (key === action.type) {
      return storeDefMap[key].reducer(state, action);
    }
  }
  return state;
};

export const combineStoreDefs = <O: {}>(storeDefMap: O): * => ({
  actions: makeNamespacedActionsFromStoreDefMap(storeDefMap),
  reducer: makeReducerFromStoreDefMap(storeDefMap),
});
