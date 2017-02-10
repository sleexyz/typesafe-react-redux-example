// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0 */
import { assert } from 'chai';
import { createStore } from 'redux';
import { combineStoreDefs, makeStoreDef } from './';

// Namespaced actions must enforce existence statically
() => {
  const FooStoreDef = makeStoreDef('hello', (state: string) => ({
    hello() { return state + state; },
  }));
  const BarStoreDef = makeStoreDef(1, (state: number) => ({
    world() { return state + 2; },
  }));
  const combinedStoreDef = combineStoreDefs({
    foo: FooStoreDef,
    bar: BarStoreDef,
  });
  const { reducer, actions } = combinedStoreDef;
  const store = createStore(reducer);
  store.dispatch(actions.foo.hello());
  // $ExpectError
  store.dispatch(actions.foo.helloo());
};

// Namespaced actions must enforce proper usage statically
() => {
  const FooStoreDef = makeStoreDef('hello', (state: string) => ({
    hello(x: number) { return x.toString() + state; },
  }));
  const BarStoreDef = makeStoreDef(1, (state: number) => ({
    world() { return state + 2; },
  }));
  const combinedStoreDef = combineStoreDefs({
    foo: FooStoreDef,
    bar: BarStoreDef,
  });
  const { reducer, actions } = combinedStoreDef;
  const store = createStore(reducer);
  store.dispatch(actions.foo.hello(1));
  // $ExpectError
  store.dispatch(actions.foo.hello('hello'));
};

// Namespaced actions must enforce proper usage statically, even with clashing
() => {
  const FooStoreDef = makeStoreDef('hello', (state: string) => ({
    hello(x: number) { return x.toString() + state; },
  }));
  const BarStoreDef = makeStoreDef(1, (state: number) => ({
    hello(x: string) { return state + x.length; },
  }));
  const combinedStoreDef = combineStoreDefs({
    foo: FooStoreDef,
    bar: BarStoreDef,
  });
  const { reducer, actions } = combinedStoreDef;
  const store = createStore(reducer);
  store.dispatch(actions.foo.hello(1));
  // $ExpectError
  store.dispatch(actions.bar.hello(1));
  // $ExpectError
  store.dispatch(actions.foo.hello('hello'));
  store.dispatch(actions.bar.hello('hello'));
};

// GetState enforces the right shape
() => {
  const FooStoreDef = makeStoreDef('hello', (state: string) => ({
    hello(x: number) { return x.toString() + state; },
  }));
  const BarStoreDef = makeStoreDef(1, (state: number) => ({
    hello(x: string) { return state + x.length; },
  }));
  const combinedStoreDef = combineStoreDefs({
    foo: FooStoreDef,
    bar: BarStoreDef,
  });
  const { reducer, actions } = combinedStoreDef;
  const store = createStore(reducer);
  (store.getState().foo: string);
  // $ExpectError
  (store.getState().foo: number);
  (store.getState().bar: number);
  // $ExpectError
  (store.getState().bar: string);
};

describe('combineStoreDefs', () => {
  it('works with no action clashes', () => {
    const FooStoreDef = makeStoreDef('hello', (state: string) => ({
      hello() { return state + state; },
    }));
    const BarStoreDef = makeStoreDef(1, (state: number) => ({
      world() { return state + 2; },
    }));
    const combinedStoreDef = combineStoreDefs({
      foo: FooStoreDef,
      bar: BarStoreDef,
    });
    const { reducer, actions } = combinedStoreDef;
    const store = createStore(reducer);
    assert.deepEqual({ foo: 'hello', bar: 1 }, store.getState());
    store.dispatch(actions.foo.hello());
    assert.deepEqual({ foo: 'hellohello', bar: 1 }, store.getState());
    store.dispatch(actions.bar.world());
    assert.deepEqual({ foo: 'hellohello', bar: 3 }, store.getState());
  });
  it('works even with action name clashes', () => {
    const FooStoreDef = makeStoreDef('hello', (state: string) => ({
      hello() { return state + state; },
    }));
    const BarStoreDef = makeStoreDef(1, (state: number) => ({
      hello() { return state + 2; },
    }));
    const combinedStoreDef = combineStoreDefs({
      foo: FooStoreDef,
      bar: BarStoreDef,
    });
    const { reducer, actions } = combinedStoreDef;
    const store = createStore(reducer);
    assert.deepEqual({ foo: 'hello', bar: 1 }, store.getState());
    store.dispatch(actions.foo.hello());
    assert.deepEqual({ foo: 'hellohello', bar: 1 }, store.getState());
    store.dispatch(actions.bar.hello());
    assert.deepEqual({ foo: 'hellohello', bar: 3 }, store.getState());
  });
});
