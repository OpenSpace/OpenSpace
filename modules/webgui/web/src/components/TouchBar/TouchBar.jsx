import React from 'react';
import FocusMenu from './FocusMenu/FocusMenu';
import Markers from './Markers';
import styles from './TouchBar.scss';

const TouchBar = () => (
  <div className={styles.TouchBar}>
    <FocusMenu />
    <Markers />
  </div>
);

export default TouchBar;
