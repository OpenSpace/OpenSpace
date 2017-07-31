import React from 'react';
import PropTypes from 'prop-types';

import styles from './Icon.scss';

/**
 * Create a Material Design icon. https://material.io/icons/
 * @param props - the props
 * @returns {XML}
 * @constructor
 */
const Icon = (props) => {
  const { icon, className } = props;
  const classNames = className.split(' ')
                            .map(s => styles[s] || s)
                            .concat(styles.base)
                            .join(' ');
  return (
    <i {...props} className={classNames}>{ icon }</i>
  );
};

Icon.propTypes = {
  icon: PropTypes.string.isRequired,
  className: PropTypes.string,
};

Icon.defaultProps = {
  className: '',
};

export default Icon;
