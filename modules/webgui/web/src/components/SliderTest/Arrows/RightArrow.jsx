import React from 'react';
import Icon from '../../common/Icon/Icon';
import styles from './Arrows.scss';

const RightArrow = ({ nextSlide }) => {
  return (
    <div className={styles.RightArrow} onClick={nextSlide}>
      <Icon icon="keyboard_arrow_right" className={styles.Icon}/>
    </div>
  )
}

export default RightArrow;