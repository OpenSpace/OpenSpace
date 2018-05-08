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
    // console.log('Picked story', e.target.id);
    //** TODO: Functionality when picking a story **//
  }

  render() {
    const { image } = this.props;
    const name = (this.props.image.split("/img/").pop()).slice(0, -4);

    return (
      <div className={styles.Container}>
        <img src={this.props.image} className={styles.Slide} alt={'Story'} />
        <div className={styles.StoryInfo}>
          <CenteredLabel className={styles.StoryName}>{name}</CenteredLabel>
          <CenteredLabel className={styles.Description}>Description about story goes here.</CenteredLabel>
          <StoryButton 
            pickStory={this.handleStory}
            storyName={name}
          />
        </div>
      </div>
    );
  }
}

Slide.propTypes = {
  image: PropTypes.string.isRequired,
};

export default Slide;
