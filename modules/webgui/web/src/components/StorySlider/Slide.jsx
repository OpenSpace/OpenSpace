import React, { Component } from 'react';
import styles from './Slider.scss';

const Slide = (props) => {
  
const currentSlide = props.background[props.currentSlide];
console.log(currentSlide);

const style = {
    imageBackground: {
      backgroundImage: `url(${currentSlide}.jpg)`,
      backgroundSize: 'cover',
      backgroundPosition: 'center center'
    }
  }

  /*
  return (
    <div className={styles.Slide} style={style.imageBackground}></div>
  )*/

  return (
    <div className={styles.Slide}>
        <img src={currentSlide}/>
    </div>
  );
}

export default Slide;