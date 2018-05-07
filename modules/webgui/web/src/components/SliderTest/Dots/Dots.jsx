import React from 'react';
import Dot from './Dot';
import styles from './Dots.scss';

const Dots = ({ index, imagePaths, dotClick}) => {
    // Map the dots and the images
    const dotsGroup = imagePaths.map((image, i) => {
    
    let active = (i === index) ? true : false;

    return (
      <Dot
        key={i}
        storyId={i}
        active={active}
        dotClick={dotClick}
      />
    )
  })

  return (
    <div className={styles.DotsContainer}>
      { dotsGroup }
    </div>
  )
}

export default Dots;