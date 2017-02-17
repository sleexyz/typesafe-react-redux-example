const makeRun = (actions) => function* (action) {
  if (action.type in actions) {
    yield* actions[action.type](action.payload)();
  }
};

const makeActionMap = (actions) => {
  const actionMap = {};
  const keys = Object.keys(actions);
  for (let i = 0; i < keys.length; i += 1) {
    const key = keys[i];
    actionMap[key] = (payload) => ({
      payload,
      type: key,
    });
  }
  return actionMap;
};

export default (actions) => ({
  run: makeRun(actions),
  actions: makeActionMap(actions),
});
