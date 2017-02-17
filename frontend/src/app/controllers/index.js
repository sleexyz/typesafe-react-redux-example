// @flow
import * as Ship from 'redux-ship';
import * as Effects from 'app/effects';
import * as Model from 'app/model';
import * as Todo from 'app/model/Todo';

const { effects } = Effects;

/*
   Util
*/

type $AsyncActionMap = {
  // eslint-disable-next-line flowtype/no-weak-types
  [key: string]: any => void => Ship.Ship<*, *, any, *>
};

const makeRunAsyncAction =
  <O: $AsyncActionMap, Effect: *, Commit: *, State: *>
  (asyncActions: O) => function* (asyncAction: *): Ship.Ship<Effect, Commit, State, void> {
    if (asyncAction.type in asyncActions) {
      yield* asyncActions[asyncAction.type](asyncAction.payload)();
    }
  };

const makeAsyncActionMap =
  <O: $AsyncActionMap, Effect: *, Commit: *, State: *>
  (asyncActions: O): ($ObjMap<O, (v: * => void => Ship.Ship<Effect, Commit, State, *>) => *>) => {
    const asyncActionMap = {};
    const keys = Object.keys(asyncActions);
    for (let i = 0; i < keys.length; i += 1) {
      const key = keys[i];
      asyncActionMap[key] = (payload) => ({
        payload,
        type: key,
      });
    }
    return asyncActionMap;
  };

type $MakeAsyncActionsOutput<O, Effect, Commit, State> = {
  runAsyncActions: * => Ship.Ship<Effect, Commit, State, void>,
  asyncActions: $ObjMap<O, (v: * => void => Ship.Ship<Effect, Commit, State, *>) => *>,
};

const makeAsyncActions =
  <O: $AsyncActionMap, Effect: *, Commit: *, State: *>
  (asyncActions: O): $MakeAsyncActionsOutput<O, Effect, Commit, State> => ({
    runAsyncActions: makeRunAsyncAction(asyncActions),
    asyncActions: makeAsyncActionMap(asyncActions),
  });

// we don't care if we lose type information in the commit...
type $Output<State> = $MakeAsyncActionsOutput<*, *, *, State>;

/*
   Application code
*/

export const { runAsyncActions, asyncActions }: $Output<Model.Type> = makeAsyncActions({
  foo: () => function* () {
    console.log('0');
    yield* effects.wait(2000);
    console.log('1');
    yield* effects.wait(2000);
    console.log('2');
    yield* effects.wait(2000);
    yield* effects.wait('hello');
    const state = yield* Ship.getState(Todo.lens.view);
    console.log(state);

    // $ExpectError
    () => effects.wit(2000);
    // $ExpectError
    () => state.Todo.asdf;
  },
});
