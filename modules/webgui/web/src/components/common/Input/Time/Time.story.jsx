/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Time from './Time';

/* globals module */

storiesOf('Input/Time', module)
  .addDecorator(story => (
    <div style={{ background: '#d8d8d8' }}>
      { story() }
    </div>
  ))
  .add('default', () => (<Time time={new Date()} />))
  .add('only hours', () => (<Time time={new Date()} elements={[Time.Elements.Hours]} />))
  .add('all', () => (<Time time={new Date()} elements={Time.Elements.All} />))
  .add('with callback', () => (<Time time={new Date()} onChange={action('changed')} />));
