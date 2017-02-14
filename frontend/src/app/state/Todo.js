// @flow
import { makeStateDef } from 'state-def';

export const namespace = 'Todo';

export type Entry = {
  value: string,
  id: number,
};

export type State = {
  nextId: number,
  todos: Array<Entry>,
};

const initialState: State = {
  nextId: 0,
  todos: [],
};

export const selectors = {
  main: ({ todos }: State) => ({ todos }),
};

const makeStateFunctions = (state: State) => ({
  createTodo: () => {
    return {
      nextId: state.nextId + 1,
      todos: [
        ...state.todos,
        { value: '', id: state.nextId },
      ],
    };
  },
  removeTodo: (index: number) => {
    const newTodos = [...state.todos];
    newTodos.splice(index, 1);
    return {
      ...state,
      todos: newTodos,
    };
  },
  updateTodo: ({ index, value }: { index: number, value: string }) => {
    const newTodos = [...state.todos];
    const oldTodo = state.todos[index];
    newTodos.splice(index, 1, { ...oldTodo, value });
    return {
      ...state,
      todos: newTodos,
    };
  },
});

export const stateDef = makeStateDef({ namespace, initialState, makeStateFunctions });
export const { actions } = stateDef;
