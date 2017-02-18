// @flow
import makeActions from './makeActions';
import makeEffects from './makeEffects';
import makeModelDef from './makeModelDef';
import ReducerCombiner from './ReducerCombiner';
import {
  identity,
  makePropertyLens,
  makeLenses,
} from './lens';
import type {
  $Reducer,
  $Commit,
  $Lens,
} from './core';

export {
  makeActions,
  makeEffects,
  makeModelDef,
};

export {
  ReducerCombiner,
};

export {
  identity,
  makePropertyLens,
  makeLenses,
};

export type {
  $Reducer,
  $Commit,
  $Lens,
};
