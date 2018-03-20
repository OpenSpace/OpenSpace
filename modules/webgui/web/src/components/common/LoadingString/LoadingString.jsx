import React from 'react';
import PropTypes from 'prop-types';

import styles from './LoadingString.scss';

const LoadingString = ({ children, loading }) => (
  <span className={loading ? styles.loading : styles.loaded}>
    { children }
  </span>
);

LoadingString.propTypes = {
  children: PropTypes.node,
  loading: PropTypes.bool,
};

LoadingString.defaultProps = {
  children: 'Loading...',
  loading: false,
};

export default LoadingString;
