import React from 'react';
import PropTypes from 'prop-types';
import styles from './Slide.scss';

const Slide = ({ image }) => (
  <div>
    <img src={image} className={styles.Slide} alt={'Story'} />
  </div>
);

Slide.propTypes = {
  image: PropTypes.string.isRequired,
};

export default Slide;
