// @flow
import { makeStoreDef } from 'store-def';

const initialState = {
  nextId: 1,
  todos: [
    { value: 'write todomvc', id: 0 },
  ],
};

const makeStateFunctions = (state: typeof initialState) => ({
  createTodo() {
    return {
      nextId: state.nextId + 1,
      todos: [
        ...state.todos,
        { value: '', id: state.nextId },
      ],
    };
  },
  removeTodo(index: number) {
    const newTodos = [...state.todos];
    newTodos.splice(index, 1);
    return {
      ...state,
      todos: newTodos,
    };
  },
  updateTodo({ index, value }: { index: number, value: string }) {
    const newTodos = [...state.todos];
    const oldTodo = state.todos[index];
    newTodos.splice(index, 1, { ...oldTodo, value });
    return {
      ...state,
      todos: newTodos,
    };
  },
});

export default makeStoreDef(initialState, makeStateFunctions);
