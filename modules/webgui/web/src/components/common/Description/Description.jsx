import React from 'react';
import PropTypes from 'prop-types';

import styles from './Description.scss';

const getClassNames = (inheritedClassName) => {
  let classes = styles.base

  if (inheritedClassName) {
    classes += ` ${inheritedClassName}`
  }

  return classes
}

const Description = (props) => {
  const { children } = props;
  const inheritedClassName = props.className;

  return (
    <span {...props} className={getClassNames(inheritedClassName)}>
      {children}
    </span>
  );
};

Description.propTypes = {
  children: PropTypes.node,
};

Description.defaultProps = {
  children: [],
};

export default Description;
