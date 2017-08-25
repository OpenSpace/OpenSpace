/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import LoadingString from './LoadingString';

/* globals module */

storiesOf('LoadingString', module)
  .addDecorator(story => (
    <div style={{ background: '#252525', padding: '20px', color: 'white' }}>
      { story() }
    </div>
  ))
  .add('Loading', () => (<LoadingString loading>With text</LoadingString>))
  .add('Loading with default text content', () => (<LoadingString loading />))
  .add('loaded', () => (<LoadingString>Animates in!</LoadingString>));
