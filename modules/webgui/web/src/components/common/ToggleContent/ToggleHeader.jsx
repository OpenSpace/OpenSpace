import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../Icon/Icon';

import styles from './ToggleHeader.scss';

const ToggleHeader = ({ title, toggled, onClick, onIcon, offIcon }) => (
  <header className={styles.toggle} onClick={onClick} role="button" tabIndex={0}>
    <span className="title">
      { title }
    </span>
    <Icon
      icon={toggled ? onIcon : offIcon}
      className={styles.icon}
    />
  </header>
);

ToggleHeader.propTypes = {
  offIcon: PropTypes.string,
  onClick: PropTypes.func.isRequired,
  onIcon: PropTypes.string,
  title: PropTypes.string.isRequired,
  toggled: PropTypes.bool.isRequired,
};

ToggleHeader.defaultProps = {
  offIcon: 'chevron_left',
  onIcon: 'expand_more',
};

export default ToggleHeader;
