// @flow
import * as Ship from 'redux-ship';
import { makeActions } from 'typesafe-redux';
import { effects } from 'app/effects';
import * as Todo from 'app/model/Todo';
import type { Action } from 'app/redux';

export const { run, actions } = makeActions({
  displayWelcome: () => function* (): Action<void> {
    yield* effects.wait(1000);
    yield* Ship.commit(Todo.commits.updateTodo({
      index: 0,
      value: 'typesafe frontend yo',
    }));
  },
});
