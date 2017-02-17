// @flow
import type {
  $ProtoCommits,
  $CommitsMap,
  $Reducer,
  $StateDef,
} from './types';

// FIXME: Supertype is essentially any...
const makeInitializeState =
  <State: {}>(initialState: State) =>
    <SuperState: $Supertype<State>>(state: SuperState): SuperState =>
      ({ ...state, ...initialState });

const makeCommits =
  <State, ProtoCommits: $ProtoCommits<State>>
  (namespace: string, protoCommits: ProtoCommits): $CommitsMap<State, ProtoCommits> => {
    const keys = Object.keys(protoCommits);
    const commits = {};
    for (let i = 0; i < keys.length; i += 1) {
      const key = keys[i];
      commits[key] = (payload, error) => ({
        error,
        payload,
        type: `${namespace}/${key}`,
      });
    }
    return commits;
  };

const makeReducer =
  <State, ProtoCommits: $ProtoCommits<State>>
  (namespace: string, protoCommits: ProtoCommits): $Reducer<State> => {
    const keys = Object.keys(protoCommits);
    const reducer = (state, { type, payload, error }) => {
      for (let i = 0; i < keys.length; i += 1) {
        const key = keys[i];
        const expectedType = `${namespace}/${keys[i]}`;
        if (expectedType === type) {
          return protoCommits[key](payload, error)(state);
        }
      }
      return state;
    };
    return reducer;
  };

type MakeStateDefOutput<State, ProtoCommits> = {
  stateDef: $StateDef<State>,
  commits: $CommitsMap<State, ProtoCommits>,
};

const makeStateDef =
  <State: {}, ProtoCommits: $ProtoCommits<State>>
  (
    namespace: string,
    initialState: $Shape<State>,
    protoCommits: ProtoCommits,
  ): MakeStateDefOutput<State, ProtoCommits> => ({
    stateDef: {
      namespace,
      reducer: makeReducer(namespace, protoCommits),
      initializeState: makeInitializeState(initialState),
    },
    commits: makeCommits(namespace, protoCommits),
  });

export default makeStateDef;
