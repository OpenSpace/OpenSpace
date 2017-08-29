import React from 'react';
import PropTypes from 'prop-types';
import styles from './Tooltip.scss';
import { excludeKeys } from '../../../utils/helpers';

const Tooltip = (props) => {
  const { children, placement, staticPosition } = props;
  return (
    <div
      {...excludeKeys(props, 'placement')}
      className={`${styles.tooltip} ${styles[placement]} ${staticPosition && styles.static}`}
    >
      { children }
    </div>
  );
};

Tooltip.propTypes = {
  children: PropTypes.node.isRequired,
  placement: PropTypes.oneOf('top right bottom left'.split(' ')),
  staticPosition: PropTypes.bool,
};

Tooltip.defaultProps = {
  placement: 'top',
  staticPosition: false,
};

export default Tooltip;
