import React, { Component } from 'react';
import PropTypes from 'prop-types';
import CenteredLabel from '../common/CenteredLabel/CenteredLabel';
import StoryButton from './StoryButton';
import styles from './Slide.scss';

class Slide extends Component {
  constructor(props) {
    super(props);

    this.handleStory = this.handleStory.bind(this);
  }

  handleStory(e) {
    this.props.onChangeStory(e.target.id);
  }

  render() {
    const { image, storyInfo } = this.props;

    return (
      <div className={styles.Container}>
        <img src={image} className={styles.Slide} alt={'Story'} />
        <div className={styles.StoryInfo}>
          <CenteredLabel className={styles.StoryName}>{storyInfo.title}</CenteredLabel>
          <CenteredLabel className={styles.Description}>{storyInfo.info}</CenteredLabel>
          <StoryButton
            pickStory={this.handleStory}
            storyIdentifier={storyInfo.identifier}
          />
        </div>
      </div>
    );
  }
}

Slide.propTypes = {
  image: PropTypes.string.isRequired,
  onChangeStory: PropTypes.func.isRequired,
  storyInfo: PropTypes.shape({
    title: PropTypes.string,
    info: PropTypes.string,
  }).isRequired,
};

export default Slide;
