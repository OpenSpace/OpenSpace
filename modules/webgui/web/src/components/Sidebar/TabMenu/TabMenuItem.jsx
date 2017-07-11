import React from 'react';
import PropTypes from 'prop-types';

import styles from './TabMenuItem.scss';

const TabMenuItem = ({ children, onClick, active }) => {
  const activeClass = active ? styles.active : '';
  return (
    <div onClick={onClick} className={`${styles.TabMenuItem} ${activeClass}`} role="tab" tabIndex={0}>
      {children}
    </div>
  );
};

TabMenuItem.propTypes = {
  children: PropTypes.node.isRequired,
  active: PropTypes.bool.isRequired,
  onClick: PropTypes.func,
};

TabMenuItem.defaultProps = {
  onClick: (() => {}),
};

export default TabMenuItem;
