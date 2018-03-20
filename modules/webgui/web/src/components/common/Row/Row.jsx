import React from 'react';
import PropTypes from 'prop-types';
import styles from './Row.scss';

const Row = props => (
  <div {...props} className={`${styles.row} ${props.className}`}>
    { props.children }
  </div>
);

Row.propTypes = {
  children: PropTypes.node.isRequired,
  className: PropTypes.string,
};

Row.defaultProps = {
  className: '',
};

export default Row;
