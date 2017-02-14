// @flow
import type {
  $ProtoActions,
  $ActionsMap,
  $Reducer,
  $StateDef,
} from './types';

const makeInitializeState =
  <State: {}>(initialState: State) =>
    <SuperState: $Supertype<State>>(state: SuperState): SuperState =>
      ({ ...state, ...initialState });

const makeActions =
  <State, ProtoActions: $ProtoActions<State>>
  (namespace: string, protoActions: ProtoActions): $ActionsMap<State, ProtoActions> => {
    const keys = Object.keys(protoActions);
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
  <State, ProtoActions: $ProtoActions<State>>
  (namespace: string, protoActions: ProtoActions): $Reducer<State, ProtoActions> => {
    const keys = Object.keys(protoActions);
    const reducer = (state, { type, payload, error }) => {
      for (let i = 0; i < keys.length; i += 1) {
        const key = keys[i];
        const expectedType = `${namespace}/${keys[i]}`;
        if (expectedType === type) {
          return protoActions[key](payload, error)(state);
        }
      }
      return state;
    };
    return reducer;
  };

type MakeStateDefOutput<State, ProtoActions> = {
  stateDef: $StateDef<State, ProtoActions>,
  actions: $ActionsMap<State, ProtoActions>,
};

const makeStateDef =
  <State: {}, ProtoActions: $ProtoActions<State>>
  (
    namespace: string,
    initialState: $Shape<State>,
    protoActions: ProtoActions,
  ): MakeStateDefOutput<State, ProtoActions> => ({
    stateDef: {
      namespace,
      reducer: makeReducer(namespace, protoActions),
      initializeState: makeInitializeState(initialState),
    },
    actions: makeActions(namespace, protoActions),
  });

export default makeStateDef;
