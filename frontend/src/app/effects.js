// @flow

import fetch from 'isomorphic-fetch';
import * as Ship from 'redux-ship';

/*
   Util
*/

type $EffectMap = {
  [key: string]: * => Promise<*>
};

const makeRunEffects = <O: $EffectMap>(effects: O) => async function (effect: *) {
  if (effect.type in effects) {
    return effects[effect.type](effect);
  }
  return null;
};

const makeEffectActions = <O: $EffectMap>(effects: O) => {
  const effectMap = {};
  const keys = Object.keys(effects);
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    effectMap[key] = (payload) => Ship.call({
      payload,
      type: key,
    });
  }
  return effectMap;
};

type $MakeEffectsOutput<O> = {
  effects: $ObjMap<O, <V>(v: V) => *>,
  runEffects: *,
}

const makeEffects = <O: $EffectMap>(effects: O): $MakeEffectsOutput<O> => ({
  effects: makeEffectActions(effects),
  runEffects: makeRunEffects(effects),
});

/*
   Code
*/

const rawEffects = {
  fetch,
  wait: (n) => new Promise((res) => {
    setTimeout(res, n);
  }),
};

export const { effects, runEffects } = makeEffects(rawEffects);
