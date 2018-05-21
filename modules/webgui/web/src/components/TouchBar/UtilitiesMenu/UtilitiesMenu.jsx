import React from 'react';
import HelpButton from './HelpButton';
import HomeButtonContainer from './HomeButtonContainer';
import Controllers from './Controllers';

import styles from './UtilitiesMenu.scss';


const UtilitiesMenu = () => (
  <div className={styles.UtilitiesMenu}>
    <HomeButtonContainer />
    <HelpButton />
    <Controllers />
  </div>
);

export default UtilitiesMenu;
