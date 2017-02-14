// @flow

type Lens<S, T> = {
  view: S => T,
  edit: (T => T) => S => S,
};

type $ToLens<S> = <T>(v: T) => Lens<S, T>;

export const makeLenses = <S: {}>(obj: S): $ObjMap<S, $ToLens<S>> => {
  const keys = Object.keys(obj);
  const lensDict = {};
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    const lens = {
      view: (x: S) => x[key],
      edit: (f) => (x: S) => ({ ...x, [key]: f(x[key]) }),
    };
    lensDict[key] = lens;
  }
  return lensDict;
};
