/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Calendar from './Calendar';

/* globals module */

const yesterDay = new Date();
yesterDay.setDate(yesterDay.getDay() - 1);

storiesOf('Calendar', module)
  .add('default', () => (<Calendar />))
  .add('with selected day', () => (<Calendar selectedDay={yesterDay} />))
  .add('with callback', () => (<Calendar selectedDay={yesterDay} onChange={action('changed')} />));
