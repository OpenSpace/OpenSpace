import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore } from 'redux';
// AppContainer is a necessary wrapper component for hot module reloading
import { AppContainer } from 'react-hot-loader';
import openspaceApp from './api/Reducers';
import middleware from './api/Middleware';
import App from './App';

/* global document */

const store = createStore(
  openspaceApp,
  middleware,
);

const render = (Component) => {
  ReactDOM.render(
    <Provider store={store} >
      <AppContainer>
        <Component />
      </AppContainer>
    </Provider>,
    document.getElementById('root'),
  );
};

render(App);

// Hot Module Replacement API
if (module.hot) {
  module.hot.accept('./App', () => render(App));
}
