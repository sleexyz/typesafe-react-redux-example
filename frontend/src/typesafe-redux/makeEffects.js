import * as Ship from 'redux-ship';

const makeRun = (effects) => async function (effect) {
  if (effect.type in effects) {
    return effects[effect.type](effect.payload);
  }
  return null;
};

const makeEffectActions = (effects) => {
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

export default (effects) => ({
  effects: makeEffectActions(effects),
  run: makeRun(effects),
});
