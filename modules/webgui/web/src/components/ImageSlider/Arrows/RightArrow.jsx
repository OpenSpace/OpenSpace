import React from 'react';
import PropTypes from 'prop-types';
import Icon from '../../common/Icon/Icon';
import styles from './Arrows.scss';

const RightArrow = ({ nextSlide }) => (
  <div className={styles.RightArrow} >
    <Icon icon="keyboard_arrow_right" className={styles.Icon}onClick={nextSlide} />
  </div>
);

RightArrow.propTypes = {
  nextSlide: PropTypes.func.isRequired,
};

export default RightArrow;
