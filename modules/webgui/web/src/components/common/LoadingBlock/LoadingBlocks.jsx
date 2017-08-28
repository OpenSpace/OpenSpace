import React from 'react';
import PropTypes from 'prop-types';
import LoadingBlock from './LoadingBlock';
import { excludeKeys } from '../../../utils/helpers';

// eslint-disable-next-line no-mixed-operators
const rand = (min, max) => Math.random() * (max - min) + min;

const LoadingBlocks = (props) => {
  const { min, max, fixed } = props;
  const count = fixed || Math.round(rand(min, max));
  const inherit = excludeKeys(props, 'fixed max min loading');
  return (
    <div {...inherit}>
      { Array.from(new Array(count), (_, key) => (
        <LoadingBlock key={`loading-${key}`} style={{ width: `${rand(50, 100)}%` }} />
      )) }
    </div>
  );
};

LoadingBlocks.propTypes = {
  fixed: PropTypes.number,
  max: PropTypes.number,
  min: PropTypes.number,
};

LoadingBlocks.defaultProps = {
  fixed: null,
  max: 5,
  min: 2,
};

export default LoadingBlocks;
