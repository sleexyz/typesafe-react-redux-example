/*
   Here we generate an async action runner dynamically.

   Note we don't actually care about the type of actions,
   since actions creator elimination will guarantee payloads
   to be typed correctly. Clashes are prevented by namespacing.
*/

export const run = (() => {
  const context = require.context('./', true, /.*Actions.js/);
  const keys = context.keys();
  const actionRunnersByNamespace = {};
  for (let i = 0; i < keys.length; i += 1) {
    const { namespace, run: run_ } = context(keys[i]);
    actionRunnersByNamespace[namespace] = run_;
  }
  return function* (action) {
    const head = action.type.substr(0, action.type.indexOf('/'));
    if (head in actionRunnersByNamespace) {
      yield* actionRunnersByNamespace[head](action);
    }
  };
})();
