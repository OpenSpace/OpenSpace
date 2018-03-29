import { configure } from '@storybook/react';

/* global require, module */

const req = require.context('../src/components', true, /(.)+\.story\.jsx$/);

function loadStories() {
  req.keys().forEach(req);
}

configure(loadStories, module);
