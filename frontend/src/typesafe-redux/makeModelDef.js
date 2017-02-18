// @flow

import * as Core from './core';
import { splitPath } from './utils';

type $RawCommit<StateSlice, Payload> = Payload => StateSlice => StateSlice;

type $RawCommits<StateSlice> = {
  [key: string]: $RawCommit<StateSlice, *>
};

type $ExtractCommit<StateSlice> =
  <Payload>(v: $RawCommit<StateSlice, Payload>)
    => (payload?: Payload) => Core.$Commit<Payload>;

type $CommitsMap<StateSlice, RawCommits> =
  $ObjMap<RawCommits, $ExtractCommit<StateSlice>>;

type $ModelDef<State> = {
  namespace: string,
  initializeState: State => State,
  reducer: Core.$Reducer<State>,
};

const makeInitializeState =
  <State, StateSlice> (
    initialState: StateSlice,
    lens: Core.$Lens<State, StateSlice>,
  ): (State => State) =>
    lens.edit(() => initialState);

const makeCommits =
  <State, RawCommits: $RawCommits<*>> (
    namespace: string,
    rawCommits: RawCommits,
  ): $CommitsMap<State, RawCommits> => {
    const keys = Object.keys(rawCommits);
    const commits = {};
    for (let i = 0; i < keys.length; i += 1) {
      const key = keys[i];
      commits[key] = (payload) => ({
        payload,
        type: `${namespace}/${key}`,
      });
    }
    return commits;
  };

const makeReducer =
  <State, StateSlice, RawCommits: $RawCommits<StateSlice>> (
    namespace: string,
    rawCommits: RawCommits,
    lens: Core.$Lens<State, StateSlice>,
  ): Core.$Reducer<State> => {
    const reducer = (state, { type, payload }) => {
        const [, tail] = splitPath(type);
        if (tail in rawCommits) {
        const modifySlice = rawCommits[tail](payload);
        return lens.edit(modifySlice)(state);
      }
      return state;
    };
    return reducer;
  };

type $MakeModelDefInput<State, StateSlice, RawCommits> = {
  namespace: string,
  lens: Core.$Lens<State, StateSlice>,
  initialState: StateSlice,
  rawCommits: RawCommits,
};

type $MakeModelDefOutput<State, StateSlice, RawCommits> = {
  modelDef: $ModelDef<State>,
  commits: $CommitsMap<StateSlice, RawCommits>,
};

const makeModelDef =
  <State, StateSlice, RawCommits: $RawCommits<StateSlice>>
  (
    input: $MakeModelDefInput<State, StateSlice, RawCommits>,
  ): $MakeModelDefOutput<State, StateSlice, RawCommits> => {
    const {
      namespace,
      lens,
      initialState,
      rawCommits,
    } = input;

    return {
      modelDef: {
        namespace,
        initializeState: makeInitializeState(initialState, lens),
        reducer: makeReducer(namespace, rawCommits, lens),
      },
      commits: makeCommits(namespace, rawCommits),
    };
  };

export default makeModelDef;
