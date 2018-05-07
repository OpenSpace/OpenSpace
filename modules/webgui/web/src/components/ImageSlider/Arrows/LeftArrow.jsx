import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../../common/Icon/Icon';
import styles from './Arrows.scss';

const LeftArrow = ({ prevSlide }) => (
  <div className={styles.LeftArrow} >
    <Icon icon="keyboard_arrow_left" className={styles.Icon} onClick={prevSlide} />
  </div>
);

LeftArrow.propTypes = {
  prevSlide: PropTypes.func.isRequired,
};

export default LeftArrow;
