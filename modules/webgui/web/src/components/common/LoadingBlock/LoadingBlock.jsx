import React from 'react';
import PropTypes from 'prop-types';
import styles from './LoadingBlock.scss';
import { excludeKeys } from '../../../utils/helpers';

const LoadingBlock = (props) => {
  const inherit = excludeKeys(props, 'loading');
  return (<div className={styles.block} {...inherit} />);
};

LoadingBlock.propTypes = {
  loading: PropTypes.bool,
};

LoadingBlock.defaultProps = {
  loading: false,
};

export default LoadingBlock;
