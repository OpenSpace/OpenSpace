import React, { Component } from 'react';

import '../styles/base.scss';
import styles from './App.scss';
import Sidebar from './Sidebar/Sidebar';
import BottomBar from './BottomBar/BottomBar';
import DataManager from '../api/DataManager';
import Connection from '../api/Connection';

class App extends Component {
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
    this.hoverControl = this.hoverControl.bind(this);
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

  hoverControl(event) {
    const { target, currentTarget } = event;
    if (target === currentTarget) {
      if (this.isHovered) {
        this.isHovered = false;
        console.log('blur');
      }
    } else if (!this.isHovered) {
      this.isHovered = true;
      console.log('focus');
    }
  }

  render() {
    return (
      <div className={styles.app}>
        <section className={styles.Grid__Left} onMouseMove={this.hoverControl}>
          <Sidebar />
        </section>
        <section className={styles.Grid__Right} onMouseMove={this.hoverControl}>
          <BottomBar />
        </section>
      </div>
    );
  }
}

export default App;
