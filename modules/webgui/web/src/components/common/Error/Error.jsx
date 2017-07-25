import React from 'react';
import PropTypes from 'prop-types';

import styles from './Error.scss';

const Error = ({ children, className = styles.error }) => (
  <div className={`${className} ${styles.ErrorBox}`}>
    { children }
  </div>
);

Error.propTypes = {
  children: PropTypes.node.isRequired,
  className: PropTypes.string,
};

export default Error;
