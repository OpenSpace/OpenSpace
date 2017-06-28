import React from 'react';
import styles from './App.scss';

const App = () => (
  <div className={styles.app}>
    <section className={styles.AppGrid__Left}>
      sidebar
    </section>
    <section className={styles.AppGrid__Right}>
      main
    </section>
  </div>
);

export default App;
