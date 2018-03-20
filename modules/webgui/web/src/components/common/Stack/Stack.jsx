import React from 'react';
import PropTypes from 'prop-types';
import styles from './Stack.scss';

const Stack = props => (
  <div {...props} className={`${styles.stack} ${props.className}`}>
    { props.children }
  </div>
);

Stack.propTypes = {
  children: PropTypes.node.isRequired,
  className: PropTypes.string,
};

Stack.defaultProps = {
  className: '',
};

export default Stack;
