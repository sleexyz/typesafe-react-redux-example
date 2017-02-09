const context = require.context('./', true, /_spec.js/);
context.keys().forEach((key) => {
  context(key);
});
