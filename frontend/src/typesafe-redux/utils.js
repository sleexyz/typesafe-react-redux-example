// @flow

export const splitPath = (s: string): [string, string] => {
  const i = s.indexOf('/');
  return [s.substr(0, i), s.substr(i + 1)];
};
