import React from 'react';
import PropTypes from 'prop-types';

import styles from './LoadingBlock.scss';

const LoadingBlock = props => (<div className={styles.block} {...props}></div>);

LoadingBlock.propTypes = {
  loading: PropTypes.bool,
};

LoadingBlock.defaultProps = {
  loading: false,
};

export default LoadingBlock;
