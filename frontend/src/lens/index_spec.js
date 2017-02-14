// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0 */
import { assert } from 'chai';
import { makeLenses } from './';

describe('lenses', () => {
  describe('makeLenses', () => {
    it('view works', () => {
      const state = {
        todos: [
          { value: 'helllo', id: 0 },
        ],
        nextId: 1,
      };
      const lenses = makeLenses(state);
      () => {
        // $ExpectError
        lenses.asdf;
        // $ExpectError
        lenses.nextId('hello');
      };
      assert.equal(lenses.nextId.view(state), 1);
    });

    it('edit works', () => {
      const state = {
        todos: [
          { value: 'helllo', id: 0 },
        ],
        nextId: 1,
      };
      const lenses = makeLenses(state);
      const incr = (x) => x + 1;
      const newState = lenses.nextId.edit(incr)(state);
      assert.equal(lenses.nextId.view(newState), 2);
    });
  });
});
