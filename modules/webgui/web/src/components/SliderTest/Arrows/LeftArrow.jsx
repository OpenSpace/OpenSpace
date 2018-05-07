import React from 'react';
import Icon from '../../common/Icon/Icon';
import styles from './Arrows.scss';

const LeftArrow = ({ prevSlide }) => {
  return (
    <div className={styles.LeftArrow} onClick={prevSlide}>
      <Icon icon="keyboard_arrow_left" className={styles.Icon}/>
    </div>
  )
}

export default LeftArrow;