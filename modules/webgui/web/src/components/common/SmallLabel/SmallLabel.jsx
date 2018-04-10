import React from 'react';
import PropTypes from 'prop-types';

import styles from './SmallLabel.scss';

const SmallLabel = (props) => {
  const { children } = props;
  return (
    <span {...props} className={styles.SmallLabel}>
      { children }
    </span>
  );
};

SmallLabel.propTypes = {
  children: PropTypes.node,
};

SmallLabel.defaultProps = {
  children: [],
};

export default SmallLabel;
