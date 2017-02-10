// @flow
/* eslint max-len: 0 */

import type { Action, StoreDef } from './makeStoreDef';

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

const combineStoreDefs = <O: {}>(storeDefMap: O): CombineStoreDefsOutput<O> => ({
  initialState: makeInitialStateFromStoreDefMap(storeDefMap),
  actions: makeNamespacedActionsFromStoreDefMap(storeDefMap),
  reducer: makeReducerFromStoreDefMap(storeDefMap),
});

export default combineStoreDefs;
