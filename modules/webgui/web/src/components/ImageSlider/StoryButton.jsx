import React from 'react';
import { Link } from 'react-router-dom';
import PropTypes from 'prop-types';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import styles from './StoryButton.scss';


const StoryButton = ({ pickStory, storyIdentifier }) => (
  <div className={styles.StoryButton} onClick={pickStory} id={storyIdentifier} role="button" tabIndex="0">
    <SmallLabel style={{ fontSize: '1.5rem' }} id={storyIdentifier}>Tap here to explore</SmallLabel>
  </div>
);

StoryButton.propTypes = {
  pickStory: PropTypes.func.isRequired,
  storyIdentifier: PropTypes.string.isRequired,
};

export default StoryButton;
