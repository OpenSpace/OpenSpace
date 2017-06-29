import React from 'react';

import styles from './App.scss';
import Sidebar from './Sidebar/Sidebar';
import BottomBar from './BottomBar/BottomBar';

const App = () => (
  <div className={styles.app}>
    <section className={styles.Grid__Left}>
      <Sidebar />
    </section>
    <section className={styles.Grid__Right}>
      <BottomBar />
    </section>
  </div>
);

export default App;
