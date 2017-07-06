/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import LoadingString from './LoadingString';

/* globals module */

storiesOf('LoadingString', module)
  .add('Loading', () => (
    <div style={{ background: 'blue' }}>
      <LoadingString loading>With text</LoadingString>
    </div>
  ))
  .add('Loading with default text content', () => (
    <div style={{ background: 'blue' }}>
      <LoadingString loading />
    </div>
  ))
  .add('loaded', () => (<LoadingString>Animates in!</LoadingString>));
