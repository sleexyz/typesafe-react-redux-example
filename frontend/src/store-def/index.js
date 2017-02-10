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

type StoreDef<S, O> = {
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

export const makeStoreDef = <S, O: _ActionsObj<S>> (initialState: S, makeActionsObj: S => ActionsObj<S, O>): StoreDef<S, O> => ({
  initialState,
  actions: makeActionsFromStoreDef(makeActionsObj),
  reducer: makeReducerFromStoreDef(initialState, makeActionsObj),
});

type StoreDefMap = {
  [key: string]: StoreDef<*, *>
};

type ExtractActions = <A>(v: { actions: A }) => A;

type ExtractState = <S>(v: { reducer: (state: S, action: Action<*, *>) => S }) => S;

const makeInitialStateFromStoreDefMap = <O: StoreDefMap>(storeDefMap: O): $ObjMap<O, ExtractState> => {
  const namespaces = Object.keys(storeDefMap);
  const state = {};
  for (let i = 0; i < namespaces.length; i += 1) {
    const ns = namespaces[i];
    state[ns] = storeDefMap[ns].initialState;
  }
  return state;
};

const makeNamespacedActionsFromStoreDefMap = <O: StoreDefMap>(storeDefMap: O): $ObjMap<O, ExtractActions> => {
  const namespacedActions = {};
  const namespaces = Object.keys(storeDefMap);
  for (let i = 0; i < namespaces.length; i += 1) {
    const ns = namespaces[i];
    const actions = storeDefMap[ns].actions;
    const modifiedActions = {};
    const keys = Object.keys(actions);
    for (let j = 0; j < keys.length; j += 1) {
      const key = keys[j];
      modifiedActions[key] = (payload, error) => {
        const action = actions[key](payload, error);
        action.type = `${ns}/${action.type}`;
        return action;
      };
    }
    namespacedActions[ns] = modifiedActions;
  }
  return namespacedActions;
};

type CombinedAction = {type: *, payload?: *, error?: Error};

const makeReducerFromStoreDefMap = <O: StoreDefMap>(storeDefMap: O) => (rawState: $ObjMap<O, ExtractState>, action: CombinedAction, error?: Error) => {
  let state = rawState;
  if (state == null) {
    state = makeInitialStateFromStoreDefMap(storeDefMap);
  }
  // set state
  const splitPoint = action.type.indexOf('/');
  const ns = action.type.substr(0, splitPoint);
  if (ns in storeDefMap) {
    const rest = action.type.substr(splitPoint + 1);
    state[ns] = storeDefMap[ns].reducer(state[ns], { ...action, type: rest }, error);
  }
  // return state
  return state;
};

type CombineStoreDefsOutput<O> = {
  initialState: $ObjMap<O, ExtractState>,
  actions: $ObjMap<O, ExtractActions>,
  reducer: (state: $ObjMap<O, ExtractState>, action: CombinedAction, error?: Error) => $ObjMap<O, ExtractState>,
}

export const combineStoreDefs = <O: {}>(storeDefMap: O): CombineStoreDefsOutput<O> => ({
  initialState: makeInitialStateFromStoreDefMap(storeDefMap),
  actions: makeNamespacedActionsFromStoreDefMap(storeDefMap),
  reducer: makeReducerFromStoreDefMap(storeDefMap),
});
