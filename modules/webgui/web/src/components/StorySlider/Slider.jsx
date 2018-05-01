import React, { Component } from 'react';
import Slide from './Slide';
import RightArrow from './Arrows/RightArrow';
import LeftArrow from './Arrows/LeftArrow';
import DotsGroup from './Dots/DotsGroup';
import styles from './Slider.scss';

import img1 from '../SliderTest/story1.png';

// Import all files from a given directory path
function importAll(r) {
  const images = {};
  r.keys().map((item) => { images[item.replace('./', '')] = r(item); });
  return images;
}

// Import all icon images from the given directory
const stories = importAll(require.context('../../../../../../sync/url/stories', false, /\.(png|jpe?g|svg)$/));
console.log('number of stories', Object.keys(stories).length);

/* To Do:
  => Add Dots & Arrow Funtionality
*/

export default class Slider extends Component {
  constructor(props) {
    super(props);

    // User just needs to set the name of each picture in whatever order they would like.
    // This array is passed down to the child Slide as props.
    this.state = {
      index: 1,
      background: stories[1],
    }
  }

  nextSlide() {
    console.log('nextSlide');
    this.setState({ index: this.state.index + 1 })
  }

  previousSlide() {
    console.log('previousSlide');
    this.setState({ index: this.state.index - 1 })
  }

  get background() {
    const story = stories[1];
    if (story) {
      return (
        <img src={story} />
      );
    }
    return (<Icon icon="language" className={styles.Icon} />);
  }

  render() {
    return (
      <div className={styles.Slider}>
        <Slide background={this.state.background} current={1} />
        { this.background }
        <RightArrow nextSlide={this.nextSlide} />
        <LeftArrow previousSlide={this.previousSlide} />
      </div>
    );
  }
}