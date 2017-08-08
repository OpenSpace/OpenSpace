import React from 'react';
import PropTypes from 'prop-types';
import { excludeKeys } from '../../../utils/helpers';
import styles from './Button.scss';

/* eslint-disable react/no-unused-prop-types */

const Button = (props) => {
  const specialClasses = 'block small transparent uppercase smalltext';
  const inheritProps = excludeKeys(props, specialClasses);

  // let extraClass = '';
  const extraClass = specialClasses.split(' ')
    .filter(c => props[c])
    .map(c => styles[c])
    .join(' ');

  return (
    <button {...inheritProps} className={`${props.className} ${extraClass} ${styles.button}`}>
      { props.children }
    </button>
  );
};

Button.propTypes = {
  block: PropTypes.bool,
  className: PropTypes.string,
  children: PropTypes.node.isRequired,
  small: PropTypes.bool,
  smalltext: PropTypes.bool,
  transparent: PropTypes.bool,
  uppercase: PropTypes.bool,
};

Button.defaultProps = {
  className: '',
  block: false,
  small: false,
  smalltext: false,
  transparent: false,
  uppercase: false,
};

export default Button;
