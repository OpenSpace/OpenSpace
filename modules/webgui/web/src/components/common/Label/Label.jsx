import React from 'react';
import PropTypes from 'prop-types';

import styles from './Label.scss';

const getClassNames = (size) => {
  let classes = styles.base

  switch(size) {
    case 'small':
      classes += ` ${styles.smallLabel}`
      break;
    default:
      break;
    case 'medium':
      classes += ` ${styles.mediumLabel}`
      break;
    case 'large':
      classes += ` ${styles.largeLabel}`
      break;
  }

  return classes
}

const Label = (props) => {
  const { children, size } = props;

  return (
    <span {...props} className={getClassNames(size)}>
      { children }
    </span>
  );
};

Label.propTypes = {
  children: PropTypes.node,
  size: PropTypes.oneOf(['small', 'medium', 'large'])
};

Label.defaultProps = {
  children: [],
  size: 'small'
};

export default Label;
