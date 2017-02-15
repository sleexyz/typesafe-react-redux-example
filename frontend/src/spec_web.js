import 'source-map-support/register';

const context = require.context('./', true, /_spec.js/);
context.keys().forEach(context);
