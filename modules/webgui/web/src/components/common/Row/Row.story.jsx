/* eslint import/no-extraneous-dependencies: 0 */
/* eslint react/prop-types: 0 */
/* globals module */
import React from 'react';
import { storiesOf } from '@storybook/react';
import Row from './Row';

const Block = ({ children }) => (<div style={{ background: 'lightgray' }}>{ children }</div>);

storiesOf('Row', module)
  .add('default', () => (
    <Row>
      <Block>block in a row</Block>
      <Block>another block in a row</Block>
      <Block>the third block in a row.</Block>
      <Block>nr 4. margin to the left of things!</Block>
    </Row>));
