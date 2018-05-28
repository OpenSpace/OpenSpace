import React from 'react';
import HelpButton from './presentational/HelpButton';
import HomeButtonContainer from './containers/HomeButtonContainer';
import InfoButtonController from './containers/InfoButtonContainer';
import Controllers from './containers/Controllers';

import styles from './style/UtilitiesMenu.scss';

const UtilitiesMenu = () => (
  <div className={styles.UtilitiesMenu}>
    <HomeButtonContainer />
    <HelpButton />
    <InfoButtonController />
    <Controllers />
  </div>
);

export default UtilitiesMenu;
