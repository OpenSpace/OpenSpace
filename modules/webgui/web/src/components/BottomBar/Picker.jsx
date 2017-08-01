import React from 'react';
import PropTypes from 'prop-types';

import styles from './Picker.scss';

const Picker = (props) => {
  const { children, className } = props;
  return (
    <div {...props} className={`${styles.Picker} ${className}`}>
      { children }
    </div>
  );
};

Picker.propTypes = {
  children: PropTypes.node.isRequired,
  className: PropTypes.string,
};

Picker.defaultProps = {
  className: '',
}

Picker.Title = styles.Title;
Picker.Name = styles.Name;

export default Picker;

