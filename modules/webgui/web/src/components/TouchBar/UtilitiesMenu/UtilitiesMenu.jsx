import React from 'react';
import HelpButton from './HelpButton';
import HomeButton from './HomeButton';
import TimeController from './TimeController';
import SightsController from './SightsController';

import styles from './UtilitiesMenu.scss';
import DateController from './DateController';

const UtilitiesMenu = () => (
  <div className={styles.UtilitiesMenu}>
    <HomeButton />
    <HelpButton />
    <TimeController />
    <DateController />
    <SightsController />
  </div>
);

export default UtilitiesMenu;
