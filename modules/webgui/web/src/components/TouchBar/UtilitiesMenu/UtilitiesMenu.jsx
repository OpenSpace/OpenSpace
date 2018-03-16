import React from 'react';
import HelpButton from './HelpButton';
import ResetButton from './ResetButton';

import styles from './UtilitiesMenu.scss';

const UtilitiesMenu = () => (
  <div className={styles.UtilitiesMenu}>
    <HelpButton />
    <ResetButton />
  </div>
);

export default UtilitiesMenu;
