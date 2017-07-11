/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import Icon from './Icon';

/* globals module */

storiesOf('Icon', module)
  .add('no options', () => (<Icon icon="language" />))
  .add('styling=small', () => (<Icon icon="language" styling="small" />))
  .add('styling=normal', () => (<Icon icon="language" styling="normal" />))
  .add('styling=medium', () => (<Icon icon="language" styling="medium" />))
  .add('styling=large', () => (<Icon icon="language" styling="large" />))
  .add('styling=extralarge', () => (<Icon icon="language" styling="extralarge" />));
