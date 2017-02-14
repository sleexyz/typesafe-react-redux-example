// @flow
/* eslint no-unused-expressions: 0, no-unused-vars: 0, max-len: 0 */
import { assert } from 'chai';
import { createStore } from 'redux';
import { makeStateDef } from './';

/*
   Flowtype tests
*/

// makeStateDef's action state is consistent with inital state, for simple types
// makeStateDef's action state is consistent with initial state, for complex object types
// makeStateDef expects state to be returned in an action
// makeStateDef forbids nonexistent actions from being called
// makeStateDef forbids improper action usage
// Redux's store.getState enforces the right shape
// Invalid Selectors are forbidden
// Valid Selectors must typecheck and enforce proper usage
