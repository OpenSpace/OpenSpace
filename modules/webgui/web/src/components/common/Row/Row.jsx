import React from 'react';
import PropTypes from 'prop-types';
import styles from './Row.scss';

const Row = props => (
  <div {...props} className={styles.row}>
    { props.children }
  </div>
);

Row.propTypes = {
  children: PropTypes.node.isRequired,
};

export default Row;
