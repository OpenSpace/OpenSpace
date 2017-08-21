import React, { Component } from 'react';
import { HashRouter as Router, Route, Link } from 'react-router-dom';
import '../styles/base.scss';
import styles from './OnScreenGui.scss';
import Sidebar from '../components/Sidebar/Sidebar';
import BottomBar from '../components/BottomBar/BottomBar';
import Error from '../components/common/Error/Error';
import Overlay from '../components/common/Overlay/Overlay';
import Connection from '../api/Connection';
import DataManager from '../api/DataManager';
import About from './About/About';
import Stack from '../components/common/Stack/Stack';

class OnScreenGui extends Component {
  constructor(props) {
    super(props);

    this.state = {
      url: Connection.DEFAULT_URL,
      isConnected: false,
      connectionLost: false,
      connectionWait: 1000,
    };

    this.connection = DataManager.connection;
    this.initializeConnection = this.initializeConnection.bind(this);
    this.resetConnection = this.resetConnection.bind(this);
    this.connectionStatusCallback = this.connectionStatusCallback.bind(this);
  }

  componentDidMount() {
    this.initializeConnection();
  }

  initializeConnection() {
    this.connection.addStatusCallback(this.connectionStatusCallback);
  }

  resetConnection() {
    this.connection = new Connection(this.state.url);
    DataManager.connection = this.connection;
    this.initializeConnection();
  }

  connectionStatusCallback(connection, event, origin) {
    switch (origin) {
      case 'onOpen':
        // everything is all right!
        this.setState({ isConnected: true, connectionLost: false, connectionWait: 1000 });
        break;
      case 'onClose':
        this.setState({ isConnected: false, connectionLost: true });
        this.tryToReconnect();
        break;
      case 'onError':
        break;
      default:
        // unknown
    }
  }

  /**
   * start reconnection attempts, at intervals ever increasing
   */
  tryToReconnect() {
    let { connectionWait } = this.state;

    console.log('Attempting to connect in', connectionWait, 'ms.'); // eslint-disable-line
    setTimeout(() => {
      DataManager.connection.reconnect();
      connectionWait *= 2;
      this.setState({ connectionWait });
    }, connectionWait);
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

        { this.state.connectionLost && (
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

export default OnScreenGui;
