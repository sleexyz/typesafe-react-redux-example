// @flow
import type {
  $Action,
  $StateDef,
} from './types';

type $StateDefMap = {
  [key: string]: $StateDef<*, *>
};

type $ExtractActions =
  <A>(v: { actions: A }) => A;

type $ExtractState = <S>(v: { reducer: (state: S, action: $Action<*, *>) => S }) => S;

type $CombinedState<O> = $ObjMap<O, $ExtractState>;

type $ExtractSelector<O> =
  <S>(v: { reducer: (state: S, action: $Action<*, *>) => S }) => $CombinedState<O> => S;

type $CombinedAction = {type: *, payload?: *, error?: Error};

type $CombinedStateDefs<O> = {
  initialState: $CombinedState<O>,
  actions: $ObjMap<O, $ExtractActions>,
  reducer: (state: $ObjMap<O, $ExtractState>, action: $CombinedAction, error?: Error)
    => $ObjMap<O, $ExtractState>,
  selectors: $ObjMap<O, $ExtractSelector<O>>,
};

const makeInitialStateFromStateDefMap =
  <StateDefMap: $StateDefMap>
  (stateDefMap: StateDefMap): $ObjMap<StateDefMap, $ExtractState> => {
    const namespaces = Object.keys(stateDefMap);
    const state = {};
    for (let i = 0; i < namespaces.length; i += 1) {
      const ns = namespaces[i];
      state[ns] = stateDefMap[ns].initialState;
    }
    return state;
  };

const makeNamespacedActionsFromStateDefMap =
  <StateDefMap: $StateDefMap>
  (stateDefMap: StateDefMap): $ObjMap<StateDefMap, $ExtractActions> => {
    const namespacedActions = {};
    const namespaces = Object.keys(stateDefMap);
    for (let i = 0; i < namespaces.length; i += 1) {
      const ns = namespaces[i];
      const actions = stateDefMap[ns].actions;
      const modifiedActions = {};
      const keys = Object.keys(actions);
      for (let j = 0; j < keys.length; j += 1) {
        const key = keys[j];
        modifiedActions[key] = (payload, error) => {
          const action = actions[key](payload, error);
          action.type = `${ns}/${key}`;
          return action;
        };
      }
      namespacedActions[ns] = modifiedActions;
    }
    return namespacedActions;
  };

const makeReducerFromStateDefMap =
  <StateDefMap: $StateDefMap>
  (stateDefMap: StateDefMap) =>
    (rawState: $ObjMap<StateDefMap, $ExtractState>, action: $CombinedAction, error?: Error) => {
      // initialize state
      let state;
      if (rawState == null) {
        state = makeInitialStateFromStateDefMap(stateDefMap);
      } else {
        state = { ...rawState };
      }
      // set state
      const splitPoint = action.type.indexOf('/');
      const ns = action.type.substr(0, splitPoint);
      if (ns in stateDefMap) {
        const rest = action.type.substr(splitPoint + 1);
        state[ns] = stateDefMap[ns].reducer(state[ns], { ...action, type: rest }, error);
      }
      // return state
      return state;
    };

const makeSelectorsFromStateDefMap =
  <StateDefMap: $StateDefMap>
  (stateDefMap: StateDefMap) => {
    const selectors = {};
    const namespaces = Object.keys(stateDefMap);
    for (let i = 0; i < namespaces.length; i += 1) {
      const ns = namespaces[i];
      selectors[ns] = (state) => state[ns];
    }
    return selectors;
  };

const combineStateDefs =
  <StateDefMap: {}>
  (stateDefMap: StateDefMap): $CombinedStateDefs<StateDefMap> => ({
    initialState: makeInitialStateFromStateDefMap(stateDefMap),
    actions: makeNamespacedActionsFromStateDefMap(stateDefMap),
    reducer: makeReducerFromStateDefMap(stateDefMap),
    selectors: makeSelectorsFromStateDefMap(stateDefMap),
  });

export default combineStateDefs;
