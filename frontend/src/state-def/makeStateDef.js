// @flow
/* eslint max-len: 0 */
import type {
  $StateFunctionMap,
  $ActionsMap,
  $Reducer,
  $StateDef,
} from './types';

// $ExpectError
const makeInitializeState = <State: {}>(initialState: $Shape<State>) => (state: State): State => ({ ...state, ...initialState });

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

type $StateDefInput<State, StateFunctionMap> = {
  namespace: string,
  initialState: $Shape<State>,
  makeStateFunctions: State => StateFunctionMap,
};

const makeStateDef =
  <State, StateFunctionMap: $StateFunctionMap<State>>
  (
   input: $StateDefInput<State, StateFunctionMap>,
  ): $StateDef<State, StateFunctionMap> => {
    const {
      namespace,
      initialState,
      makeStateFunctions,
    } = input;
    return {
      namespace,
      initializeState: makeInitializeState(initialState),
      actions: makeActions(namespace, makeStateFunctions),
      reducer: makeReducer(namespace, makeStateFunctions),
    };
  };

export default makeStateDef;
