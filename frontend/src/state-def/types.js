// @flow

import type { Reducer } from 'redux';

export type $ProtoAction<State, Payload> = Payload => State => State;

export type $ProtoActions<State> = {
  [key: string]: * => State => State,
};

export type $Action<Payload, ProtoActions> = {
  type: $Keys<ProtoActions>,
  payload?: Payload,
  error?: Error,
};

export type $ActionCreator<Payload, ProtoActions> =
  (payload?: Payload, error?: Error) => $Action<Payload, ProtoActions>;

export type $ActionsMap<State, ProtoActions> =
  $ObjMap<ProtoActions, $ExtractActionFromProtoAction<State, ProtoActions>>;

type $ExtractActionFromProtoAction<State, ProtoActions> =
  <Payload>(v: $ProtoAction<State, Payload>) => $ActionCreator<Payload, ProtoActions>

// eslint-disable-next-line flowtype/no-weak-types
export type $Reducer<State, ProtoActions> = Reducer<State, $Action<any, ProtoActions>>;

export type $StateDef<State: {}, ProtoActions> = {
  namespace: string,
  initializeState: State => State,
  reducer: $Reducer<State, ProtoActions>,
};
