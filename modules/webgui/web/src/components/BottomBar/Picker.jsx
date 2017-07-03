import React from 'react';
import PropTypes from 'prop-types';

import styles from './Picker.scss';

const Picker = (props) => {
  const { children } = props;
  return (
    <div {...props} className={styles.Picker}>
      { children }
    </div>
  );
};

Picker.propTypes = {
  children: PropTypes.node.isRequired,
};

Picker.Title = styles.Title;
Picker.Name = styles.Name;

export default Picker;

