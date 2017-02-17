// @flow
import { makeStateDef } from 'typesafe-redux';
import { makeLenses } from 'lens';

export type Entry = {
  value: string,
  id: number,
};

export type State = {
  nextId: number,
  todos: Array<Entry>,
};

export type Slice = { Todo: State };

const initialSlice: Slice = {
  Todo: {
    nextId: 1,
    todos: [
      { value: '', id: 0 },
    ],
  },
};

export const { Todo: lens } = makeLenses(initialSlice);

const rawCommits = {
  createTodo: () => lens.edit((state: State) => {
    return {
      nextId: state.nextId + 1,
      todos: [
        ...state.todos,
        { value: '', id: state.nextId },
      ],
    };
  }),
  removeTodo: (index: number) => (slice: Slice) => {
    if (lens.view(slice).todos.length === 1) {
      const newSlice = rawCommits.removeTodoInternal(index)(slice);
      return rawCommits.createTodo(index)(newSlice);
    }
    return rawCommits.removeTodoInternal(index)(slice);
  },
  removeTodoInternal: (index: number) => lens.edit((state: State) => {
    const newTodos = [...state.todos];
    newTodos.splice(index, 1);
    return {
      ...state,
      todos: newTodos,
    };
  }),
  updateTodo: ({ index, value }) => lens.edit((state: State) => {
    const newTodos = [...state.todos];
    const oldTodo = state.todos[index];
    newTodos.splice(index, 1, { ...oldTodo, value });
    return {
      ...state,
      todos: newTodos,
    };
  }),
};

export const { commits, stateDef } = makeStateDef('Todo', initialSlice, rawCommits);
