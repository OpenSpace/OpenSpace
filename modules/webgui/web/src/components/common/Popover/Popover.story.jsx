/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Popover from './Popover';

/* globals module */

storiesOf('Popover', module)
  .add('default (arrow bottom center)', () => (<Popover>Hello!</Popover>))
  .add('arrow bottom left', () => (<Popover arrow="arrow bottom leftside">Hello!</Popover>))
  .add('arrow bottom right', () => (<Popover arrow="arrow bottom rightside">Hello!</Popover>))
  .add('small', () => (<Popover arrow="arrow bottom rightside" className="small">Hello!</Popover>))
  .add('with title', () => (<Popover title="title" closeCallback={action('close')}>Hello!</Popover>))
  .add('no arrow', () => (<Popover arrow="">Hello!</Popover>));
