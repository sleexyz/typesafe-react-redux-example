// @flow
/* eslint max-len: 0 */
import type {
  $StateFunctionMap,
  $ActionsMap,
  $Reducer,
  $StateDef,
} from './types';

const makeActionsFromStateDef =
  <State, StateFunctionMap: $StateFunctionMap<State>>
  (makeStateFunctionMap: State => StateFunctionMap): $ActionsMap<State, StateFunctionMap> => {
    const keys = Object.keys((makeStateFunctionMap: any)()); // eslint-disable-line flowtype/no-weak-types
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

const makeReducerFromStateDef =
  <State, StateFunctionMap: $StateFunctionMap<State>>
  (initialState: State, makeStateFunctionMap: State => StateFunctionMap): $Reducer<State, StateFunctionMap> => {
    const stateFunctionMap = {};
    const keys = Object.keys((makeStateFunctionMap: any)()); // eslint-disable-line flowtype/no-weak-types
    for (let i = 0; i < keys.length; i += 1) {
      const key = keys[i];
      stateFunctionMap[key] = (state, payload, error) => makeStateFunctionMap(state)[key](payload, error);
    }
    const reducer = (state = initialState, { type, payload, error }) => {
      for (let i = 0; i < keys.length; i += 1) {
        const key = keys[i];
        if (key === type) {
          return stateFunctionMap[key](state, payload, error);
        }
      }
      return state;
    };
    return reducer;
  };

type $StateDefInput<State, StateFunctionMap> = {
  initialState: State,
  makeStateFunctions: State => StateFunctionMap,
  // TODO: consider switching to something like
  // stateFunctions: { [key: String]: (State, *) => State }
};

const makeStateDef =
  <State, StateFunctionMap: $StateFunctionMap<State>>
  ({ initialState, makeStateFunctions }: $StateDefInput<State, StateFunctionMap>): $StateDef<State, StateFunctionMap> => ({
    initialState,
    actions: makeActionsFromStateDef(makeStateFunctions),
    reducer: makeReducerFromStateDef(initialState, makeStateFunctions),
  });

export default makeStateDef;
