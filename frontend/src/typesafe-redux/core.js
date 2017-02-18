// @flow

export type $Commit<Payload> = {
  type: $Subtype<string>,
  payload?: Payload,
};

export type $Reducer<State> = (State, $Commit<*>) => State;

export type $Lens<S, T> = {
  view: S => T,
  edit: (T => T) => (S => S),
};
