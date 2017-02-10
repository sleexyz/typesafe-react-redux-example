const context = require.context('./', true, /_spec.js/);
context.keys().forEach((key) => {
  const requireWithContext = context;
  requireWithContext(key);
});
