import React from 'react';
import HelpButton from './HelpButton';
import HomeButtonContainer from './HomeButtonContainer';
import Controllers from './Controllers';

import styles from './UtilitiesMenu.scss';
import InfoButtonController from './InfoButtonContainer';


const UtilitiesMenu = () => (
  <div className={styles.UtilitiesMenu}>
    <HomeButtonContainer />
    <HelpButton />
    <InfoButtonController />
    <Controllers />
  </div>
);

export default UtilitiesMenu;
