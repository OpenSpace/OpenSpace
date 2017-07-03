import React from 'react';
import PropTypes from 'prop-types';

import styles from './Icon.scss';

/**
 * Create a Material Design icon. https://material.io/icons/
 * @param icon      - the name of the icon
 * @param styling   - optional string of styling class names, see Icon.scss for options.
 * @returns {XML}
 * @constructor
 */
const Icon = ({ icon, styling = [] }) => {
  const classNames = styling.split(' ')
                            .map(s => styles[s] || s)
                            .concat(styles.base)
                            .join(' ');
  return (
    <i className={classNames}>{ icon }</i>
  );
};

Icon.propTypes = {
  icon: PropTypes.string.isRequired,
  styling: PropTypes.string,
};

Icon.defaultProps = {
  styling: '',
};

export default Icon;
