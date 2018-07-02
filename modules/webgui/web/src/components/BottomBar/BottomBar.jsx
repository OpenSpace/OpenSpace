import React from 'react';

import OriginPicker from './Origin/OriginPicker';
import TimePicker from './TimePicker';
import TfEditor from './TfEditor/containers/TfEditor';
import DataLoader from '../DataLoader/DataLoader';
import styles from './BottomBar.scss';

const BottomBar = () => (
  <div className={styles.BottomBar}>
    <TfEditor />
    <OriginPicker />
    <TimePicker />
    <DataLoader />
  </div>
);

export default BottomBar;
