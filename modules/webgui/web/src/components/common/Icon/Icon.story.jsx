/* eslint import/no-extraneous-dependencies: 0 */
import React from 'react';
import { storiesOf } from '@storybook/react';
import Icon from './Icon';

/* globals module */

storiesOf('Icon', module)
  .add('no options', () => (<Icon icon="language" />))
  .add('className=small', () => (<Icon icon="language" className="small" />))
  .add('className=normal', () => (<Icon icon="language" className="normal" />))
  .add('className=medium', () => (<Icon icon="language" className="medium" />))
  .add('className=large', () => (<Icon icon="language" className="large" />))
  .add('className=extralarge', () => (<Icon icon="language" className="extralarge" />));
