import 'source-map-support/register';
import { jsdom } from 'jsdom';

global.document = jsdom('');
global.window = document.defaultView;
Object.keys(document.defaultView).forEach((property) => {
  if (typeof global[property] === 'undefined') {
    global[property] = document.defaultView[property];
  }
});
global.navigator = {
  userAgent: 'node.js',
};

const context = require.context('./', true, /_spec.js/);
context.keys().forEach(context);
