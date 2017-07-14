/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import Popover from './Popover';

/* globals module */

storiesOf('Popover', module)
  .add('default (arrow bottom center)', () => (<Popover>Hello!</Popover>))
  .add('arrow bottom left', () => (<Popover arrow="arrow bottom leftside">Hello!</Popover>))
  .add('arrow bottom right', () => (<Popover arrow="arrow bottom rightside">Hello!</Popover>))
  .add('no arrow', () => (<Popover arrow="">Hello!</Popover>));
