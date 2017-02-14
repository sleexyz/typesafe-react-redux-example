// @flow
import { makeStateDef } from 'state-def';
import { makeLenses } from 'lens';

export type Entry = {
  value: string,
  id: number,
};

export type State = {
  nextId: number,
  todos: Array<Entry>,
};

const initialState = {
  Todo: {
    nextId: 0,
    todos: [],
  },
};

export const { Todo: TodoLens } = makeLenses(initialState);

export const { actions, stateDef } = makeStateDef('Todo', initialState, {
  createTodo: () => TodoLens.edit((state: State) => {
    return {
      nextId: state.nextId + 1,
      todos: [
        ...state.todos,
        { value: '', id: state.nextId },
      ],
    };
  }),
  removeTodo: (index: number) => TodoLens.edit((state: State) => {
    const newTodos = [...state.todos];
    newTodos.splice(index, 1);
    return {
      ...state,
      todos: newTodos,
    };
  }),
  updateTodo: ({ index, value }) => TodoLens.edit((state: State) => {
    const newTodos = [...state.todos];
    const oldTodo = state.todos[index];
    newTodos.splice(index, 1, { ...oldTodo, value });
    return {
      ...state,
      todos: newTodos,
    };
  }),
});
