import React from 'react';
import HelpButton from './HelpButton';
import HomeButton from './HomeButton';
import TimeController from './TimeController';

import styles from './UtilitiesMenu.scss';

const UtilitiesMenu = () => (
  <div className={styles.UtilitiesMenu}>
    <HomeButton />
    <HelpButton />
    <TimeController />
  </div>
);

export default UtilitiesMenu;
