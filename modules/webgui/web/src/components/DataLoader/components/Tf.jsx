import React from 'react';
import PropTypes from 'prop-types';
import styles from './Tf.scss';

const Tf = (props) => {
  const selectedClass = props.selected ? styles.selected : '';

  return (
    <div
      className={`${styles.tf} ${selectedClass}`}
      onClick={() => props.onClick(props.path)}
      role="presentation"
    >
      <img
        key={props.path}
        src={props.img}
        alt="Transfer function preset"
        className={`${styles.image} ${selectedClass}`}
      />
      <div className={styles.meta}>
        <p>{props.min} </p>
        <p>to</p>
        <p>{props.max}</p>
      </div>
    </div>
  );
};

Tf.propTypes = {
  onClick: PropTypes.func,
  path: PropTypes.string,
  img: PropTypes.string,
  selected: PropTypes.bool,
  min: PropTypes.string,
  max: PropTypes.string,
};

Tf.defaultProps = {
  onClick: () => {},
  path: '',
  img: '',
  selected: false,
  min: '0.0',
  max: '0.0',
};

export default Tf;

