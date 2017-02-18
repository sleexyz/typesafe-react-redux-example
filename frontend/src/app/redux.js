// @flow
import * as Ship from 'redux-ship';
import * as Model from 'app/model';

export type Action<O> = Ship.Ship<*, *, Model.State, O>;
