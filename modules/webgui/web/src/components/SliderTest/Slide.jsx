import React, { Component } from 'react';
import styles from './Slide.scss';

const Slide = ({ image }) => {
  return (
    <div>
        <img src={image} className={styles.Slide} alt={'Story'} />
    </div>
  );
}

export default Slide;