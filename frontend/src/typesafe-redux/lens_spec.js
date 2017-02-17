// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0 */
import { expect } from 'chai';
import { makePropertyLens, makeLenses } from './';
import * as Core from './core';

describe('lenses', () => {
  describe('makePropertyLens', () => {
    const state = {
      todos: [
        { value: 'helllo', id: 0 },
      ],
      nextId: 1,
    };
    const lens: Core.$Lens<{ nextId: number }, number> = makePropertyLens('nextId');

    it('view works', () => {
      expect(lens.view(state)).to.equal(1);
    });

    it('edit works', () => {
      expect(lens.edit((x) => x + 1)(state)).to.deep.equal({
        todos: [
          { value: 'helllo', id: 0 },
        ],
        nextId: 2,
      });
    });
  });

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
      expect(lenses.nextId.view(state)).to.equal(1);
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
      expect(lenses.nextId.view(newState)).to.equal(2);
    });
  });
});
