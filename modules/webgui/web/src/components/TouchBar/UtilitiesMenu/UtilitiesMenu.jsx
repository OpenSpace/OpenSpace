import React from 'react';
import HelpButton from './HelpButton';
import ResetButton from './ResetButton';
import TimeController from './TimeController';

import styles from './UtilitiesMenu.scss';

const UtilitiesMenu = () => (
  <div className={styles.UtilitiesMenu}>
    <HelpButton />
    <ResetButton />
    <TimeController />
  </div>
);

export default UtilitiesMenu;
