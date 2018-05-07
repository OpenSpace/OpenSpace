import React from 'react';
import styles from './Dots.scss';

const Dot = ({ storyId, active, dotClick }) => {
  const dotStyle = active ? styles.active : styles.Dot;

  return <div className={dotStyle} onClick={() => dotClick(storyId)} />
}

export default Dot;