// @flow
import { expect } from 'chai';
import { splitPath } from './utils';

describe('utils', () => {
  describe('splitPath', () => {
    it('works', () => {
      expect(splitPath('foo/bar')).to.deep.equal([
        'foo',
        'bar',
      ]);
    });
  });
})
