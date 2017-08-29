import React from 'react';
import PropTypes from 'prop-types';
import styles from './Tooltip.scss';
import { excludeKeys } from '../../../utils/helpers';

const Tooltip = (props) => {
  const { children, placement, fixed } = props;
  return (
    <div
      {...excludeKeys(props, 'placement fixed')}
      className={`${styles.tooltip} ${styles[placement]} ${fixed && styles.fixed}`}
    >
      { children }
    </div>
  );
};

Tooltip.propTypes = {
  children: PropTypes.node.isRequired,
  placement: PropTypes.oneOf('top right bottom left'.split(' ')),
  fixed: PropTypes.bool,
};

Tooltip.defaultProps = {
  placement: 'top',
  fixed: false,
};

export default Tooltip;
