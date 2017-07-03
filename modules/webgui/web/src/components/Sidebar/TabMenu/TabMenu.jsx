import React from 'react';
import PropTypes from 'prop-types';

import styles from './TabMenu.scss';

const TabMenu = ({ children }) => (
  <div className={styles.TabMenu}>
    { children }
  </div>
);

TabMenu.propTypes = {
  children: PropTypes.node.isRequired,
};

export default TabMenu;
