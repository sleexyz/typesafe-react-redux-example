// @flow

/*
   (parametrized) mixins are Capitalized.
   dictionaries are lowercased.
*/

export const colors = {
  blue: 'blue',
  grey: '#252756',
  transgrey: 'rgba(0, 0, 0, 0.50)',
};

export const Card = () => `
border: 1px solid ${colors.transgrey};
`;

export const Button = () => `
border: none;
background: none;
color: ${colors.transgrey};
`;
