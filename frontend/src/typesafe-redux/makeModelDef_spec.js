// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0, max-len: 0 */
import { expect } from 'chai';
import { createStore } from 'redux';
import { makeModelDef } from './';
import * as Core from './core';
import * as Lens from './lens';

/*
   Note we only test type inference here.
   We test reducer functionality in ReducerCombiner.
*/

// commit dict shape is enforced
() => {
  const { commits, modelDef } = makeModelDef({
    namespace: 'test',
    lens: (Lens.makePropertyLens('test'): Core.$Lens<{ modelDef1: string }, string>),
    initialState: 'hello',
    rawCommits: {
      replaceWith: (x: string) => (state) => {
        return x;
      },
      append: (x: string) => (state) => {
        return state + x;
      },
      foo: () => (state) => {
        return state;
      },
    },
  });
  // $ExpectError
  commits.asdf();
  // $ExpectError
  commits.replaceWith();
  // $ExpectError
  commits.append(2);
};

describe('makeModelDef', () => {
  it('creates valid, namespaced commits', () => {
    const { commits, modelDef } = makeModelDef({
      namespace: 'test',
      lens: (Lens.makePropertyLens('test'): Core.$Lens<{ modelDef1: string }, string>),
      initialState: 'hello',
      rawCommits: {
        replaceWith: (x: string) => () => {
          return x;
        },
        append: (x: string) => (state) => {
          return state + x;
        },
      },
    });
    expect(commits.replaceWith('hello')).to.deep.equal({
      type: 'test/replaceWith',
      payload: 'hello',
    });
  });
});
