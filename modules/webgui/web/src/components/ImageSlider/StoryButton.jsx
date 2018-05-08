import React from 'react';
import PropTypes from 'prop-types';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import styles from './StoryButton.scss';

const StoryButton = ({ pickStory, storyName }) => (
    <div className={styles.StoryButton} onClick={pickStory} id={storyName} role="button" tabIndex="0">
        <SmallLabel id={storyName}>Tap here to explore {storyName}</SmallLabel>
    </div>
);

StoryButton.propTypes = {
  pickStory: PropTypes.func.isRequired,
  storyName: PropTypes.string.isRequired,
};

export default StoryButton;