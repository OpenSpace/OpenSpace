/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Input from './Input';

/* globals module */

storiesOf('Input.Input', module)
  .add('default', () => (<Input placeholder="Input" value="PREDEFined value" />))
  .add('no input', () => (<Input placeholder="Input" />))
  .add('wide', () => (<Input placeholder="Input" wide />))
  .add('disabled', () => (<Input placeholder="Input" value="value" disabled />))
  .add('with callback', () =>
    (<Input placeholder="Input" value="change me" onChange={action('input')} />));
