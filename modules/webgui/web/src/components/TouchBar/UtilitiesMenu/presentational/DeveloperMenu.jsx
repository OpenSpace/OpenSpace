import React from 'react';
import PropTypes from 'prop-types';
import styles from './../style/DeveloperMenu.scss';
import Button from '../../../../components/common/Input/Button/Button';

const DeveloperMenu = (props) => {
  const json = require('../../../../../../../../data/assets/stories/stories.json');
  const buttons = json.stories.map(story => (
    <Button
      onClick={props.changeStory}
      id={story.identifier}
      key={story.identifier}
      className={`${styles.developerButtons}
        ${props.storyIdentifier === story.identifier && styles.active}`}
    >
      {story.identifier}
    </Button>
  ));
  return (
    <div className={styles.menuContainer}>
      {buttons}
    </div>
  );
};

DeveloperMenu.propTypes = {
  changeStory: PropTypes.func.isRequired,
  storyIdentifier: PropTypes.string.isRequired,
};

export default DeveloperMenu;
