import React from 'react';
import ReactDOM from 'react-dom';
// AppContainer is a necessary wrapper component for hot module reloading
import { AppContainer } from 'react-hot-loader';

import App from './components/App';

/* global document */

const render = (Component) => {
  ReactDOM.render(
    <AppContainer>
      <Component />
    </AppContainer>,
    document.getElementById('root'),
  );
};

render(App);

// Hot Module Replacement API
if (module.hot) {
  module.hot.accept('./components/App', () => render(App));
}
