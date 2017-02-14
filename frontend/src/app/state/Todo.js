// @flow
import { makeStateDef } from 'state-def';

export type Entry = {
  value: string,
  id: number,
};

export type State = {
  nextId: number,
  todos: Array<Entry>,
};

export const selectors = {
  main: ({ todos }: State) => ({ todos }),
};

const initialState: State = {
  nextId: 0,
  todos: [],
};

export const { actions, stateDef } = makeStateDef('Todo', {
  createTodo: () => (state) => {
    return {
      nextId: state.nextId + 1,
      todos: [
        ...state.todos,
        { value: '', id: state.nextId },
      ],
    };
  },
  removeTodo: (index: number) => (state) => {
    const newTodos = [...state.todos];
    newTodos.splice(index, 1);
    return {
      ...state,
      todos: newTodos,
    };
  },
  updateTodo: ({ index, value }: { index: number, value: string }) => (state) => {
    const newTodos = [...state.todos];
    const oldTodo = state.todos[index];
    newTodos.splice(index, 1, { ...oldTodo, value });
    return {
      ...state,
      todos: newTodos,
    };
  },
}, initialState);
