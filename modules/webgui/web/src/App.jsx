import React from 'react';
import { HashRouter as Router, Switch, Route } from 'react-router-dom';
import OnScreenGui from './views/OnScreenGui';
import OnTouchGui from './views/OnTouchGui';
import NotFound from './views/NotFound';

// TODO: add routing
const App = () => (
  <Router>
    <Switch>
      <Route path="/onscreen" component={OnScreenGui} />
      <Route path="/ontouch" component={OnTouchGui} />
      {/* Here, more GUI variations can be added. */}
      {/* <Route path="/tablet" component={TabletGui} /> */}
      <Route component={NotFound} />
    </Switch>
  </Router>
);

export default App;
