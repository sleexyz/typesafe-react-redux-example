// @flow
import fetch from 'isomorphic-fetch';
import * as Typesafe from 'typesafe-redux';

export const rawEffects = {

  fetch(x: string): Promise<Response> {
    return fetch(x);
  },

  wait(millis: number): Promise<void> {
    return new Promise((resolve) => {
      window.setTimeout(resolve, millis);
    });
  },

};

export const { effects, run } = Typesafe.makeEffects(rawEffects);

// FIXME: the following line is necessary for type inference, somehow...
effects.wait(0); // eslint-disable-line no-unused-expressions
