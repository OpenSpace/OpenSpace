/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Button from './Button';

/* globals module */

// eslint-disable-next-line
const W = ({ children }) => (<div style={{ background: '#252525', padding: '20px' }}>{ children }</div>);

storiesOf('Input.Button', module)
  .add('default', () => (<W><Button>button!</Button></W>))
  .add('with callback', () => (<W><Button onClick={action('clickety!')}>clicky button!</Button></W>))
  .add('block', () => (<W><Button block>blocky wide button!</Button></W>))
  .add('transparent', () => (<W><Button transparent>transparent button!</Button></W>))
  .add('small', () => (<W><Button small>small button!</Button></W>))
  .add('smalltext', () => (<W><Button smalltext>smalltext button!</Button></W>))
  .add('disabled', () => (<W><Button disabled>disabled button!</Button></W>))
  .add('uppercase', () => (<W><Button uppercase>uppercase button!</Button></W>))
  .add('ALL the variations', () =>
    (<W><Button small smalltext block uppercase transparent>crazy button!</Button></W>));
