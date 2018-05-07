import React from 'react';
import PropTypes from 'prop-types';
import Dot from './Dot';
import styles from './Dots.scss';

const Dots = ({ index, imagePaths, dotClick }) => {
  // Map the dots and the images
  const dotsGroup = imagePaths.map((image, i) => {
    const active = (i === index);
    return (
      <Dot
        key={image}
        storyId={i}
        active={active}
        dotClick={dotClick}
      />
    );
  });

  return (
    <div className={styles.DotsContainer}>
      { dotsGroup }
    </div>
  );
};

Dots.defaultProps = {
  imagePaths: null,
};

Dots.propTypes = {
  index: PropTypes.number.isRequired,
  imagePaths: PropTypes.arrayOf(PropTypes.string),
  dotClick: PropTypes.func.isRequired,
};

export default Dots;
