/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Button from './Button';

/* globals module */

storiesOf('Input.Button', module)
  .add('default', () => (<Button>button!</Button>))
  .add('with callback', () => (<Button onClick={action('clickety!')}>clicky button!</Button>))
  .add('block', () => (<Button block>blocky wide button!</Button>))
  .add('transparent', () => (<Button transparent>transparent button!</Button>))
  .add('small', () => (<Button small>small button!</Button>))
  .add('smalltext', () => (<Button smalltext>smalltext button!</Button>))
  .add('disabled', () => (<Button disabled>disabled button!</Button>))
  .add('uppercase', () => (<Button uppercase>uppercase button!</Button>));
