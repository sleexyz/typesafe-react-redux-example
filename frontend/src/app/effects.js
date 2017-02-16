// @flow

import * as Ship from 'redux-ship';
import fetch from 'isomorphic-fetch';
import * as Todo from 'app/model/Todo';

// idea: define controller and effect type in tandem

const sto = (n) => new Promise((res) => {
  setTimeout(res, n);
});

export async function run(effect: *) {
  const res = await fetch('http://localhost:8000/todos');
  const text = await res.text();
  await sto(1000);
  console.log(text);
  await sto(1000);
  console.log(text);
  await sto(1000);
  console.log(text);
  return text;
}

run();
