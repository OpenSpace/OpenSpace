/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Checkbox from './Checkbox';

/* globals module */

storiesOf('Input/Checkbox', module)
  .add('default', () => (<Checkbox label="Input" />))
  .add('checked', () => (<Checkbox label="Input" checked />))
  .add('left', () => (<Checkbox label="Input" left />))
  .add('unwide', () => (<Checkbox label="Input" wide={false} />))
  .add('with callback', () => (<Checkbox label="Input" onChange={action('changed')} />));
