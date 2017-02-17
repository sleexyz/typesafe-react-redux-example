// @flow
import * as Typesafe from 'typesafe-redux';

// TODO: derive model schema from swagger
const namespace = 'Todo';

export type Entry = {
  value: string,
  id: number,
};

export type State = {
  nextId: number,
  todos: Array<Entry>,
};

const initialState: State = {
  nextId: 1,
  todos: [
    { value: '', id: 0 },
  ],
};

export const lens: Typesafe.$Lens<{ Todo: State}, State> = Typesafe.makePropertyLens('Todo');

const rawCommits = {

  createTodo: () => (state: State) => {
    return {
      nextId: state.nextId + 1,
      todos: [
        ...state.todos,
        { value: '', id: state.nextId },
      ],
    };
  },

  removeTodo: (index: number) => (state: State) => {
    const newTodos = [...state.todos];
    newTodos.splice(index, 1);
    return {
      ...state,
      todos: newTodos,
    };
  },

  updateTodo: (params: {| index: number, value: string |}) => (state: State) => {
    const { index, value } = params;
    const newTodos = [...state.todos];
    const oldTodo = state.todos[index];
    newTodos.splice(index, 1, { ...oldTodo, value });
    return {
      ...state,
      todos: newTodos,
    };
  },

};

export const { commits, modelDef } = Typesafe.makeModelDef({
  namespace,
  lens,
  initialState,
  rawCommits,
});
