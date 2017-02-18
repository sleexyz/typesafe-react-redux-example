import { splitPath } from './utils';

const makeRun = (namespace, rawActions) => function* ({ type, payload }) {
  const [, tail] = splitPath(type);
  if (tail in rawActions) {
    yield* rawActions[tail](payload)();
  }
};

const makeActionMap = (namespace, rawActions) => {
  const actionMap = {};
  const keys = Object.keys(rawActions);
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    actionMap[key] = (payload) => ({
      payload,
      type: `${namespace}/${key}`,
    });
  }
  return actionMap;
};

export default ({ namespace, rawActions }) => ({
  namespace,
  run: makeRun(namespace, rawActions),
  actions: makeActionMap(namespace, rawActions),
});
