// @flow
import * as Core from './core';

export const makePropertyLens = (key: $Subtype<string>): Core.$Lens<*, *> => ({
  view: (x) => x[key],
  edit: (f) => (x) => ({ ...x, [key]: f(x[key]) }),
});

type $ToLens<S> = <T>(v: T) => Core.$Lens<S, T>;

export const makeLenses = <S: {}>(obj: S): $ObjMap<S, $ToLens<S>> => {
  const keys = Object.keys(obj);
  const lensDict = {};
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    lensDict[key] = makePropertyLens(key);
  }
  return lensDict;
};
