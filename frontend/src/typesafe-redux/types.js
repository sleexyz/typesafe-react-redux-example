// @flow

import type { Reducer } from 'redux';

export type $ProtoCommit<State, Payload> = Payload => State => State;

export type $ProtoCommits<State> = {
  [key: string]: $ProtoCommit<State, *>
};

// TODO: get rid of Error
export type $Commit<Payload> = {
  type: $Subtype<string>,
  payload?: Payload,
  error?: Error,
};

export type $CommitsMap<State, ProtoCommits> =
  $ObjMap<ProtoCommits, $ExtractCommit<State>>;

type $ExtractCommit<State> =
  <Payload>(v: $ProtoCommit<State, Payload>)
    => (payload?: Payload, error?: Error) => $Commit<Payload>;

// eslint-disable-next-line flowtype/no-weak-types
export type $Reducer<State> = Reducer<State, $Commit<any>>;

export type $StateDef<State: {}> = {
  namespace: string,
  initializeState: State => State,
  reducer: $Reducer<State>,
};
