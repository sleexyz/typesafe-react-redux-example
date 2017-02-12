// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0 */
import { assert } from 'chai';
import { createStore } from 'redux';
import { combineStateDefs, makeStateDef } from './';

/*
   Flowtype tests
*/

// Namespaced actions must enforce existence statically
() => {
  const FooStateDef = makeStateDef({
    initialState: 'hello',
    makeStateFunctions: (state: string) => ({
      hello() { return state + state; },
    }),
    selectors: {},
  });
  const BarStateDef = makeStateDef({
    initialState: 1,
    makeStateFunctions: (state: number) => ({
      world() { return state + 2; },
    }),
    selectors: {},
  });
  const combinedStateDef = combineStateDefs({
    foo: FooStateDef,
    bar: BarStateDef,
  });
  const { reducer, actions } = combinedStateDef;
  const store = createStore(reducer);
  store.dispatch(actions.foo.hello());
  // $ExpectError
  store.dispatch(actions.foo.helloo());
  // $ExpectError
  store.dispatch(actions.fxoo.hello());
};

// Namespaced actions must enforce proper usage statically
() => {
  const FooStateDef = makeStateDef({
    initialState: 'hello',
    makeStateFunctions: (state: string) => ({
      hello(x: number) { return x.toString() + state; },
    }),
    selectors: {},
  });
  const BarStateDef = makeStateDef({
    initialState: 1,
    makeStateFunctions: (state: number) => ({
      world() { return state + 2; },
    }),
    selectors: {},
  });
  const combinedStateDef = combineStateDefs({
    foo: FooStateDef,
    bar: BarStateDef,
  });
  const { reducer, actions } = combinedStateDef;
  const store = createStore(reducer);
  store.dispatch(actions.foo.hello(1));
  // $ExpectError
  store.dispatch(actions.foo.hello('hello'));
};

// Namespaced actions must enforce proper usage statically, even with potential clashing
() => {
  const FooStateDef = makeStateDef({
    initialState: 'hello',
    makeStateFunctions: (state: string) => ({
      hello(x: number) { return x.toString() + state; },
    }),
    selectors: {},
  });
  const BarStateDef = makeStateDef({
    initialState: 1,
    makeStateFunctions: (state: number) => ({
      hello(x: string) { return state + x.length; },
    }),
    selectors: {},
  });
  const combinedStateDef = combineStateDefs({
    foo: FooStateDef,
    bar: BarStateDef,
  });
  const { reducer, actions } = combinedStateDef;
  const store = createStore(reducer);
  store.dispatch(actions.foo.hello(1));
  store.dispatch(actions.bar.hello('hello'));
  // $ExpectError
  store.dispatch(actions.bar.hello(1));
  // $ExpectError
  store.dispatch(actions.foo.hello('hello'));
};

// Redux's store.getState enforces the right shape
() => {
  const FooStateDef = makeStateDef({
    initialState: 'hello',
    makeStateFunctions: (state: string) => ({
      hello(x: number) { return x.toString() + state; },
    }),
    selectors: {},
  });
  const BarStateDef = makeStateDef({
    initialState: 1,
    makeStateFunctions: (state: number) => ({
      hello(x: string) { return state + x.length; },
    }),
    selectors: {},
  });
  const combinedStateDef = combineStateDefs({
    foo: FooStateDef,
    bar: BarStateDef,
  });
  const { reducer, actions } = combinedStateDef;
  const store = createStore(reducer);
  (store.getState().foo: string);
  (store.getState().bar: number);
  // $ExpectError
  (store.getState().foo: number);
  // $ExpectError
  (store.getState().bar: string);
};

// Selectors enforce the right shape
() => {
  const FooStateDef = makeStateDef({
    initialState: 'hello',
    makeStateFunctions: (state: string) => ({
      hello(x: number) { return x.toString() + state; },
    }),
    selectors: {},
  });
  const BarStateDef = makeStateDef({
    initialState: 1,
    makeStateFunctions: (state: number) => ({
      hello(x: string) { return state + x.length; },
    }),
    selectors: {},
  });
  const combinedStateDef = combineStateDefs({
    foo: FooStateDef,
    bar: BarStateDef,
  });
  const { reducer, actions, selectors } = combinedStateDef;
  const store = createStore(reducer);
  (selectors.foo(store.getState()): string);
  (selectors.bar(store.getState()): number);
  // $ExpectError
  (selectors.foo(store.getState()): number);
  // $ExpectError
  (selectors.bar(store.getState()): string);
};

/*
   Runtime tests
*/

describe('combineStateDefs', () => {
  it('works with no action clashes', () => {
    const FooStateDef = makeStateDef({
      initialState: 'hello',
      makeStateFunctions: (state: string) => ({
        hello() { return state + state; },
      }),
      selectors: {},
    });
    const BarStateDef = makeStateDef({
      initialState: 1,
      makeStateFunctions: (state: number) => ({
        world() { return state + 2; },
      }),
      selectors: {},
    });
    const combinedStateDef = combineStateDefs({
      foo: FooStateDef,
      bar: BarStateDef,
    });
    const { reducer, actions } = combinedStateDef;
    const store = createStore(reducer);
    assert.deepEqual({ foo: 'hello', bar: 1 }, store.getState());
    store.dispatch(actions.foo.hello());
    assert.deepEqual({ foo: 'hellohello', bar: 1 }, store.getState());
    store.dispatch(actions.bar.world());
    assert.deepEqual({ foo: 'hellohello', bar: 3 }, store.getState());
  });

  it('works even with action name clashes', () => {
    const FooStateDef = makeStateDef({
      initialState: 'hello',
      makeStateFunctions: (state: string) => ({
        hello(payload: string) { return state + payload; },
      }),
      selectors: {},
    });
    const BarStateDef = makeStateDef({
      initialState: 1,
      makeStateFunctions: (state: number) => ({
        hello() { return state + 2; },
      }),
      selectors: {},
    });
    const combinedStateDef = combineStateDefs({
      foo: FooStateDef,
      bar: BarStateDef,
    });
    const { reducer, actions } = combinedStateDef;
    const store = createStore(reducer);
    assert.deepEqual({ foo: 'hello', bar: 1 }, store.getState());
    store.dispatch(actions.foo.hello('asdf'));
    assert.deepEqual({ foo: 'helloasdf', bar: 1 }, store.getState());
    store.dispatch(actions.bar.hello());
    assert.deepEqual({ foo: 'helloasdf', bar: 3 }, store.getState());
  });

  it('returns working selectors', () => {
    const FooStateDef = makeStateDef({
      initialState: 'hello',
      makeStateFunctions: (state: string) => ({
        hello() { return state + state; },
      }),
      selectors: {},
    });
    const BarStateDef = makeStateDef({
      initialState: 1,
      makeStateFunctions: (state: number) => ({
        hello() { return state + 2; },
      }),
      selectors: {},
    });
    const combinedStateDef = combineStateDefs({
      foo: FooStateDef,
      bar: BarStateDef,
    });
    const { reducer, actions, selectors } = combinedStateDef;
    const store = createStore(reducer);
    assert.equal('hello', selectors.foo(store.getState()));
    assert.equal(1, selectors.bar(store.getState()));
  });
});
