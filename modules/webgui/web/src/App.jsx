import React from 'react';
import { HashRouter as Router, Switch, Route } from 'react-router-dom';
import OnScreenGui from './views/OnScreenGui';
import NotFound from './views/NotFound';

// TODO: add routing
const App = () => (
  <Router>
    <Switch>
      <Route path="/onscreen" component={OnScreenGui} />
      {/* Here, more GUI variations can be added. */}
      {/* <Route path="/tablet" component={TabletGui} /> */}
      <Route component={NotFound} />
    </Switch>
  </Router>
);

export default App;
