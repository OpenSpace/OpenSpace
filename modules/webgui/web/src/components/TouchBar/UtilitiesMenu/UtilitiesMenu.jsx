import React from 'react';
import HelpButton from './HelpButton';
import HomeButton from './HomeButton';
import Controllers from './Controllers';

import styles from './UtilitiesMenu.scss';


const UtilitiesMenu = () => (
  <div className={styles.UtilitiesMenu}>
    <HomeButton />
    <HelpButton />
    <Controllers />
  </div>
);

export default UtilitiesMenu;
