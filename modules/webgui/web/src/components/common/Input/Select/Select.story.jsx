/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import Select from './Select';
import ReactSelect from 'react-select';

/* globals module */

const options = 'a b c d e f g h'.split(' ').map(v => ({ value: v, label: v }));

storiesOf('Input/Select', module)
  .addDecorator(story => (
    <div style={{ background: '#252525', padding: '20px', color: 'white' }}>
      { story() }
    </div>
  ))
  .add('default', () => (<Select label="Select!" options={options} />))
  .add('searchable', () => (<Select label="Select!" options={options} searchable />))
  .add('clearable', () => (<Select label="Select!" options={options} clearable />));
