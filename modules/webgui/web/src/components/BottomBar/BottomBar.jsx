import React from 'react';

import OriginPicker from './Origin/OriginPicker';
import TimePicker from './TimePicker';
import styles from './BottomBar.scss';

const BottomBar = () => (
  <div className={styles.BottomBar}>
    <OriginPicker />
    <TimePicker />
  </div>
);

export default BottomBar;
