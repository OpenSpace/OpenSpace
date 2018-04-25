import React from 'react';
import FocusMenu from './FocusMenu/FocusMenu';
import UtilitiesMenu from './UtilitiesMenu/UtilitiesMenu';
import Markers from './Markers/Markers';
import styles from './TouchBar.scss';

const TouchBar = () => (
  <div className={styles.TouchBar}>
    <section className={styles.Grid__Left}>
      <UtilitiesMenu />
    </section>
    <section className={styles.Grid__Right}>
      <FocusMenu />
    </section>
    <Markers />
  </div>
);

export default TouchBar;
