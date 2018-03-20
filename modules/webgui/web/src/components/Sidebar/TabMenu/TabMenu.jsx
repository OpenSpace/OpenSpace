import React from 'react';
import PropTypes from 'prop-types';

import styles from './TabMenu.scss';

const TabMenu = ({ children }) => (
  <nav className={styles.TabMenu}>
    { children }
  </nav>
);

TabMenu.propTypes = {
  children: PropTypes.node.isRequired,
};

export default TabMenu;
