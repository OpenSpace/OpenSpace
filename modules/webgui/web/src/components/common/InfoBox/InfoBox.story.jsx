/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import InfoBox from './InfoBox';

/* globals module */

storiesOf('InfoBox', module)
  .addDecorator(story => (
    <div style={{ padding: '20px' }}>
      { story() }
      Hover over it!
    </div>
  ))
  .add('default', () => (<InfoBox text="text" />))
  .add('with html content', () =>
    (<InfoBox text={(<a href="http://gph.is/1PLFn5U" target="_blank">It is a link!</a>)} />))
  .add('special icon', () => (<InfoBox text="look it's a globe!" icon="language" />));
