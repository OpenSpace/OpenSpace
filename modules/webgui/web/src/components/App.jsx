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
      url: Connection.defaultUrl,
      isConnected: false,
      connectionLost: false,

    };

    this.connection = null;
    this.initializeConnection = this.initializeConnection.bind(this);
  }

  componentDidMount() {
    this.initializeConnection();
  }

  initializeConnection() {
    this.connection = new Connection(this.state.url);
    DataManager.connection = this.connection;
  }

  render() {
    return (
      <div className={styles.app}>
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

export default App;
