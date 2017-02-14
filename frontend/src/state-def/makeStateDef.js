// @flow
/* eslint max-len: 0 */
import type {
  $StateFunctionMap,
  $ActionsMap,
  $Reducer,
  $StateDef,
  $SelectorsMap,
} from './types';

const makeActions =
  <State, StateFunctionMap: $StateFunctionMap<State>>
  (
    namespace: string,
    makeStateFunctionMap: State => StateFunctionMap,
  ): $ActionsMap<State, StateFunctionMap> => {
    const keys = Object.keys((makeStateFunctionMap: any)()); // eslint-disable-line flowtype/no-weak-types
    const actions = {};
    for (let i = 0; i < keys.length; i += 1) {
      const key = keys[i];
      actions[key] = (payload, error) => ({
        error,
        payload,
        type: `${namespace}/${key}`,
      });
    }
    return actions;
  };

const makeReducer =
  <State, StateFunctionMap: $StateFunctionMap<State>>
  (
    namespace: string,
    makeStateFunctionMap: State => StateFunctionMap,
  ): $Reducer<State, StateFunctionMap> => {
    const stateFunctionMap = {};
    const keys = Object.keys((makeStateFunctionMap: any)()); // eslint-disable-line flowtype/no-weak-types
    for (let i = 0; i < keys.length; i += 1) {
      const key = keys[i];
      stateFunctionMap[key] = (state, payload, error) => makeStateFunctionMap(state)[key](payload, error);
    }
    const reducer = (state, { type, payload, error }) => {
      for (let i = 0; i < keys.length; i += 1) {
        const key = keys[i];
        const expectedType = `${namespace}/${keys[i]}`;
        if (expectedType === type) {
          return stateFunctionMap[key](state, payload, error);
        }
      }
      return state;
    };
    return reducer;
  };

type $StateDefInput<State, StateFunctionMap, SelectorsMap> = {
  namespace: string,
  initializeState: State => State,
  makeStateFunctions: State => StateFunctionMap,
  selectors: SelectorsMap,
};

const makeStateDef =
  <State, StateFunctionMap: $StateFunctionMap<State>, SelectorsMap: $SelectorsMap<State>>
  (
   input: $StateDefInput<State, StateFunctionMap, SelectorsMap>,
  ): $StateDef<State, StateFunctionMap, SelectorsMap> => {
    const {
      namespace,
      initializeState,
      makeStateFunctions,
      selectors,
    } = input;
    return {
      namespace,
      initializeState,
      selectors,
      actions: makeActions(namespace, makeStateFunctions),
      reducer: makeReducer(namespace, makeStateFunctions),
    };
  };

export default makeStateDef;
