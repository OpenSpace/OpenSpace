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
};

Picker.Active = styles.Active;
Picker.Name = styles.Name;
Picker.Popover = styles.Popover;
Picker.Title = styles.Title;
Picker.Wrapper = styles.Wrapper;
Picker.Window = styles.Window;

export default Picker;

