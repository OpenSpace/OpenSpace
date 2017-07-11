/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import SmallLabel from './SmallLabel';
/* globals module */

storiesOf('SmallLabel', module)
  .add('no options', () => (<SmallLabel>Hello</SmallLabel>))
  .add('with some props', () => (
    <SmallLabel style={{ color: 'red' }} onClick={action('clicked')}>Click me!</SmallLabel>));
