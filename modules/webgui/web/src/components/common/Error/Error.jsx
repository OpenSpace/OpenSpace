import React from 'react';
import PropTypes from 'prop-types';

import styles from './Error.scss';

const Error = ({ children, className }) => (
  <div className={`${className} ${styles.ErrorBox}`}>
    { children }
  </div>
);

Error.propTypes = {
  children: PropTypes.node.isRequired,
  className: PropTypes.string,
};

Error.defaultProps = {
  className: styles.error,
};

export default Error;
