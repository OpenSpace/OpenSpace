import React from 'react';
import { storiesOf } from '@storybook/react';
import Icon from './Icon';

/* globals module */

storiesOf('Icon', module)
  .add('no options', () => (<Icon icon="language" />))
  .add('small', () => (<Icon icon="language" styling={['small']} />))
  .add('normal', () => (<Icon icon="language" styling={['normal']} />))
  .add('medium', () => (<Icon icon="language" styling={['medium']} />))
  .add('large', () => (<Icon icon="language" styling={['large']} />))
  .add('extralarge', () => (<Icon icon="language" styling={['extralarge']} />));
