import React, { Component } from 'react';
import { withRouter, HashRouter as Router, Route, Link } from 'react-router-dom';
import { connect } from 'react-redux';
import '../styles/base.scss';
import styles from './OnScreenGui.scss';
import Sidebar from '../components/Sidebar/Sidebar';
import BottomBar from '../components/BottomBar/BottomBar';
import Error from '../components/common/Error/Error';
import Overlay from '../components/common/Overlay/Overlay';
import About from './About/About';
import Stack from '../components/common/Stack/Stack';
import { startConnection } from '../api/Actions';

class OnScreenGui extends Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
    this.props.StartConnection();
  }

  render() {
    return (
      <div className={styles.app}>
        <Router basename="/onscreen/">
          <Route
            path="/about"
            render={() => (
              <Overlay>
                <Stack style={{ maxWidth: '500px' }}>
                  <Link style={{ 'align-self': 'flex-end', color: 'white' }} to="/">
                    Close
                  </Link>
                  <About />
                </Stack>
              </Overlay>
            )}
          />
        </Router>

        { this.props.connectionLost && (
          <Overlay>
            <Error>
              Connection lost. Trying to reconnect again soon.
            </Error>
          </Overlay>
        )}
        <section className={styles.Grid__Left}>
          <Sidebar />
        </section>
        <section className={styles.Grid__Right}>
          <BottomBar />
        </section>
      </div>
    );
  }
}

const mapStateToProps = state => ({
  connectionLost: state.connection.connectionLost,
});

const mapDispatchToProps = dispatch => ({
  StartConnection: () => {
    dispatch(startConnection());
  },
});

OnScreenGui = withRouter(connect(
  mapStateToProps,
  mapDispatchToProps,
)(OnScreenGui));

export default OnScreenGui;
