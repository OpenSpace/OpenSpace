import React from 'react';
import PropTypes from 'prop-types';

import styles from './Label.scss';

const getClassNames = (size, inheritedClassName) => {
  let classes = styles.base

  if (inheritedClassName) {
    classes += ` ${inheritedClassName}`
  }

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
  const inheritedClassName = props.className;

  return (
    <span {...props} className={getClassNames(size, inheritedClassName)}>
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
  size: 'medium'
};

export default Label;
