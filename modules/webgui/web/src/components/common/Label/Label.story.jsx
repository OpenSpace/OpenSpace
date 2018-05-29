/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Label from './Label';
/* globals module */

storiesOf('Label', module)
  .add('no options', () => (<Label>Hello</Label>))
  .add('with some props', () => (
    <Label style={{ color: 'red' }} onClick={action('clicked')}>Click me!</Label>));
