const makeRun = (namespace, rawActions) => function* (action) {
  const splitPoint = action.type.indexOf('/');
  const expectedNamespace = action.type.substr(0, splitPoint);
  if (expectedNamespace in rawActions) {
    yield* rawActions[action.type](action.payload)();
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
