// @flow

import { expect } from 'chai';
import React from 'react';
import { mount } from 'enzyme';
import { Provider } from 'react-redux';
import makeStore from 'app/store';
import App from 'app/components/App';

import TodoListEntry, {
  Input as EntryInput,
  Close as EntryClose,
} from 'app/components/TodoListEntry';

const makeRoot = () => {
  return (
    <Provider store={makeStore()}>
      <App />
    </Provider>
  );
};

describe('TodoMVC', () => {
  it('renders a TodoListEntry', () => {
    const wrapper = mount(makeRoot());
    expect(wrapper.find(TodoListEntry)).to.have.length(1);
  });

  it('can add a TodoListEntry', () => {
    const wrapper = mount(makeRoot());
    wrapper.find(EntryInput).simulate('keyDown', { key: 'Enter' });
    expect(wrapper.find(TodoListEntry)).to.have.length(2);
  });

  it('can edit a TodoListEntry', () => {
    const wrapper = mount(makeRoot());
    wrapper.find(EntryInput).simulate('change', { target: { value: 'asdf' } });
    expect(wrapper.find(EntryInput).prop('value')).to.equal('asdf');
  });

  it('can add delete a TodoListEntry', () => {
    const wrapper = mount(makeRoot());
    wrapper.find(EntryInput).simulate('keyDown', { key: 'Enter' });
    expect(wrapper.find(TodoListEntry)).to.have.length(2);
    wrapper.find(EntryClose).first().simulate('click');
    expect(wrapper.find(TodoListEntry)).to.have.length(1);
  });
});
